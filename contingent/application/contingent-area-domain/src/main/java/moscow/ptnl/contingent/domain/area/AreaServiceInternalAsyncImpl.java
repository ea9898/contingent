/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package moscow.ptnl.contingent.domain.area;

import java.util.Collections;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.stream.Collectors;

import moscow.ptnl.contingent.domain.area.entity.Area;
import moscow.ptnl.contingent.domain.area.model.area.AddMedicalEmployee;
import moscow.ptnl.contingent.domain.area.model.area.AddressRegistry;
import moscow.ptnl.contingent.infrastructure.service.setting.SettingService;
import moscow.ptnl.contingent.security.RequestContext;
import moscow.ptnl.contingent.security.UserContextHolder;
import moscow.ptnl.contingent.sysop.SysopErrorReason;
import moscow.ptnl.contingent.sysop.entity.SysopMsg;
import moscow.ptnl.contingent.sysop.entity.SysopMsgParam;
import moscow.ptnl.contingent.error.ContingentException;
import moscow.ptnl.contingent.error.ValidationMessage;
import moscow.ptnl.contingent.infrastructure.service.TransactionRunService;
import moscow.ptnl.contingent.sysop.repository.SysopMsgParamRepository;
import moscow.ptnl.contingent.sysop.repository.SysopMsgRepository;
import moscow.ptnl.contingent.sysop.repository.SysopRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

/**
 * Реализация методов сервиса {@see AreaServiceInternalImpl} 
 * которые должны выполняться асинхронно.
 * 
 * Делается отдельным компонентом, так как иначе асинхронность не сработает. 
 * 
 * @author mkachalov
 */
@Service
public class AreaServiceInternalAsyncImpl implements AreaServiceInternalAsync {

    private final static Logger LOG = LoggerFactory.getLogger(AreaServiceInternalAsyncImpl.class);

    @Autowired
    private Algorithms algorithms;
    
    @Autowired
    private TransactionRunService transactionRunner;

    @Autowired
    private SysopMsgRepository sysopMsgRepository;

    @Autowired
    private SysopRepository sysopRepository;

    @Autowired
    @Lazy
    private AreaService areaServiceDomain;

    @Autowired
    @Lazy
    private EsuHelperService esuHelperService;

    @Autowired
    @Lazy
    private SettingService settingService;

    @Autowired
    private SysopMsgParamRepository sysopMsgParamRepository;

    // Асинхронная инициация процесса распределения жилых домов к территории обслуживания МО (К_УУ_27)
    @Override
    @Async 
    @Transactional(propagation = Propagation.REQUIRED, rollbackFor = Exception.class)
    public void asyncInitiateAddMoAddress(long sysopId, RequestContext requestContext, long moId, long areaTypeCode, long orderId, List<AddressRegistry> addresses) throws ContingentException {
        try {
            UserContextHolder.setContext(requestContext);
            //Выполняем в новой транзакции, чтобы можно было сохранить результат операции при ошибке
            List<Long> ids = transactionRunner.run(() -> {
                UserContextHolder.setContext(requestContext);
                return areaServiceDomain.addMoAddress(moId, areaTypeCode, orderId, addresses, false);
            }).get();
            String sysopResult = ids.stream().map(id -> id.toString()).collect(Collectors.joining(";"));
            algorithms.sysOperationComplete(sysopId, true, sysopResult);
        } catch (Throwable e) {
            processException(e, sysopId);
        }
    }
    
    // Асинхронное создание участка первичного класса (А_УУ_10)
    @Override
    @Async 
    @Transactional(propagation = Propagation.REQUIRED, rollbackFor = Exception.class)
    public void asyncCreatePrimaryArea(RequestContext requestContext, long sysopId, long moId, Long muId, Integer number, String description, Long areaTypeCode,
                                       Long areaTypeProfileCode, List<Long> policyTypes, Integer ageMin, Integer ageMax, Integer ageMinM,
                                       Integer ageMaxM, Integer ageMinW, Integer ageMaxW,
                                       boolean autoAssignForAttachment, Boolean attachByMedicalReason,
                                       List<AddMedicalEmployee> addMedicalEmployees,
                                       List<AddressRegistry> addresses) {
        try {
            UserContextHolder.setContext(requestContext);
            //Выполняем в новой транзакции, чтобы можно было сохранить результат операции при ошибке
            Long areaId = transactionRunner.run(() -> {
                UserContextHolder.setContext(requestContext);
                Area area = areaServiceDomain.createPrimaryAreaInternal(moId, muId, number, areaTypeCode, areaTypeProfileCode,
                        policyTypes, ageMin, ageMax, ageMinM, ageMaxM, ageMinW, ageMaxW, autoAssignForAttachment, attachByMedicalReason, description);
                areaServiceDomain.setMedicalEmployeeOnAreaInternal(area.getId(), addMedicalEmployees, Collections.emptyList());
                areaServiceDomain.addAreaAddressInternal(area.getId(), addresses, false);

                return area.getId();
            }).get();

            if (Boolean.TRUE.equals(settingService.getPar4())) {
                esuHelperService.sendAreaInfoEvent(areaServiceDomain.getAreaById(areaId).getArea(), "initiateCreatePrimaryArea");
            }

            algorithms.sysOperationComplete(sysopId, true, areaId.toString());
        } catch (Throwable e) {
            processException(e, sysopId);
        }
    }

    @Override
    @Async
    @Transactional(propagation = Propagation.REQUIRED, rollbackFor = Exception.class)
    public void asyncAddAreaAddress(RequestContext requestContext, long sysopId, Long areaId, List<AddressRegistry> addressesRegistry) {
        try {
            UserContextHolder.setContext(requestContext);
            //Выполняем в новой транзакции, чтобы можно было сохранить результат операции при ошибке
            List<Long> ids = transactionRunner.run(() -> {
                UserContextHolder.setContext(requestContext);
                return areaServiceDomain.addAreaAddress(areaId, addressesRegistry, false);
            }).get();
            String sysopResult = ids.stream().map(id -> id.toString()).collect(Collectors.joining(";"));
            algorithms.sysOperationComplete(sysopId, true, sysopResult);
        } catch (Throwable e) {
            processException(e, sysopId);
        }
    }
    
    private void processException(Throwable th, long sysopId) {
        if (th instanceof ExecutionException && th.getCause() != null) {
            th = th.getCause() instanceof RuntimeException && th.getCause().getCause() != null
                    ? th.getCause().getCause() : th.getCause();
        }
        if (th instanceof ContingentException) {
            for (ValidationMessage error : ((ContingentException) th).getValidation().getMessages()) {
                long sysopMsgId = sysopMsgRepository.save(new SysopMsg(
                        sysopRepository.getOne(sysopId),
                        error.getType().value(),
                        error.getCode(),
                        error.getMessage())).getId();
                List<SysopMsgParam> params = error.getParameters().stream().map(param -> new SysopMsgParam(
                        sysopMsgRepository.getOne(sysopMsgId),
                        param.getCode(),
                        param.getValue()
                )).collect(Collectors.toList());
                sysopMsgParamRepository.saveAll(params);
            }
        }
        else {
            LOG.error("Системная ошибка при выполнении асинхронного метода", th);
            String error = String.format(SysopErrorReason.UNEXPECTED_ERROR.getDescription(), th.toString()
                    + (th.getStackTrace().length > 0 ? "; " + th.getStackTrace()[0].toString() : ""));
            sysopMsgRepository.save(new SysopMsg(sysopRepository.getOne(sysopId),
                    SysopErrorReason.UNEXPECTED_ERROR.getMessageType().value(),
                    SysopErrorReason.UNEXPECTED_ERROR.getCode(),
                    error.substring(0, Math.min(error.length(), 1000))));
        }
        algorithms.sysOperationComplete(sysopId, false, null);
    }
    
}
