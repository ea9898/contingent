/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package moscow.ptnl.contingent.area.service;

import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import moscow.ptnl.contingent.area.entity.sysop.SysopMsg;
import moscow.ptnl.contingent.area.entity.sysop.SysopMsgParam;
import moscow.ptnl.contingent.area.error.SysopErrorReason;
import moscow.ptnl.contingent.error.ContingentException;
import moscow.ptnl.contingent.error.ValidationMessage;
import moscow.ptnl.contingent.repository.sysop.SysopCRUDRepository;
import moscow.ptnl.contingent.repository.sysop.SysopMsgCRUDRepository;
import moscow.ptnl.contingent.repository.sysop.SysopMsgParamCRUDRepository;
import moscow.ptnl.ws.security.RequestContext;
import moscow.ptnl.ws.security.UserContextHolder;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import ru.mos.emias.contingent2.address.AddressRegistryBaseType;
import ru.mos.emias.contingent2.core.AddMedicalEmployee;
import ru.mos.emias.errors.domain.ErrorMessageType;

/**
 * Реализация методов сервиса {@see AreaServiceInternalImpl} 
 * которые должны выполняться асинхронно.
 * 
 * Делается отдельным компонентом, так как иначе асинхронность не сработает. 
 * 
 * @author mkachalov
 */
@Service
public class AreaServiceInternalImplAsync {

    private final static Logger LOG = LoggerFactory.getLogger(AreaServiceInternalImplAsync.class);

    @Autowired
    private AreaServiceInternal areaService;
    
    @Autowired
    private Algorithms algorithms;
    
    @Autowired
    private SysopMsgCRUDRepository sysopMsgCRUDRepository;
    
    @Autowired
    private SysopMsgParamCRUDRepository sysopMsgParamCRUDRepository;
        
    @Autowired
    private SysopCRUDRepository sysopCRUDRepository;


    // Асинхронная инициация процесса распределения жилых домов к территории обслуживания МО (К_УУ_27)
    @Async 
    @Transactional(propagation = Propagation.REQUIRED, rollbackFor = Exception.class)
    public void asyncInitiateAddMoAddress(long sysopId, RequestContext requestContext, long moId, long areaTypeCode, long orderId, List<AddressRegistryBaseType> addresses) throws ContingentException {
        try {
            UserContextHolder.setContext(requestContext);
            List<Long> ids = areaService.addMoAddress(moId, areaTypeCode, orderId, addresses, false);
            String sysopResult = ids.stream().map(id -> id.toString()).collect(Collectors.joining(";"));
            algorithms.sysOperationComplete(sysopId, true, sysopResult);
        } catch (Throwable e) {
            processException(e, sysopId);
        }
    }
    
    // Асинхронное создание участка первичного класса (А_УУ_10)
    @Async 
    @Transactional(propagation = Propagation.REQUIRED, rollbackFor = Exception.class)
    public void asyncCreatePrimaryArea(RequestContext requestContext, long sysopId, long moId, Long muId, Integer number, String description, Long areaTypeCode,
                                       List<Long> policyTypes, Integer ageMin, Integer ageMax, Integer ageMinM,
                                       Integer ageMaxM, Integer ageMinW, Integer ageMaxW,
                                       boolean autoAssignForAttachment, Boolean attachByMedicalReason,
                                       List<AddMedicalEmployee> addMedicalEmployees,
                                       List<AddressRegistryBaseType> addresses) {
        try {
            UserContextHolder.setContext(requestContext);
            Long areaId = areaService.createPrimaryArea(moId, muId, number, areaTypeCode, policyTypes, ageMin, ageMax, ageMinM,
                    ageMaxM, ageMinW, ageMaxW, autoAssignForAttachment, attachByMedicalReason, description);
            areaService.setMedicalEmployeeOnArea(areaId, addMedicalEmployees, Collections.emptyList());
            areaService.addAreaAddress(areaId, addresses, false);
            algorithms.sysOperationComplete(sysopId, true, areaId.toString());            
        } catch (Throwable e) {
            processException(e, sysopId);
        }
    }
    
    @Async
    @Transactional(propagation = Propagation.REQUIRED, rollbackFor = Exception.class)
    public void asyncAddAreaAddress(RequestContext requestContext, long sysopId, Long areaId, List<AddressRegistryBaseType> addressesRegistry) {
        try {
            UserContextHolder.setContext(requestContext);
            List<Long> ids = areaService.addAreaAddress(areaId, addressesRegistry, false);
            String sysopResult = ids.stream().map(id -> id.toString()).collect(Collectors.joining(";"));
            algorithms.sysOperationComplete(sysopId, true, sysopResult);
        } catch (Throwable e) {
            processException(e, sysopId);
        }
    }
    
    private void processException(Throwable th, long sysopId) {
        if (th instanceof ContingentException) {
            for (ValidationMessage error : ((ContingentException) th).getValidation().getMessages()) {
                long sysopMsgId = sysopMsgCRUDRepository.save(new SysopMsg(
                        sysopCRUDRepository.getOne(sysopId),
                        error.getType().value(),
                        error.getCode(),
                        error.getMessage())).getId();
                List<SysopMsgParam> params = error.getParameters().stream().map(param -> new SysopMsgParam(
                        sysopMsgCRUDRepository.getOne(sysopMsgId),
                        param.getCode(),
                        param.getValue()
                )).collect(Collectors.toList());
                sysopMsgParamCRUDRepository.saveAll(params);
            }
        }
        else {
            LOG.error("Системная ошибка при выполнении асинхронного метода", th);
            String error = String.format(SysopErrorReason.UNEXPECTED_ERROR.getDescription(), th.toString()
                    + (th.getStackTrace().length > 0 ? "; " + th.getStackTrace()[0].toString() : ""));
            sysopMsgCRUDRepository.save(new SysopMsg(sysopCRUDRepository.getOne(sysopId),
                    SysopErrorReason.UNEXPECTED_ERROR.getMessageType().value(),
                    SysopErrorReason.UNEXPECTED_ERROR.getCode(),
                    error.substring(0, Math.min(error.length(), 1000))));
        }
        algorithms.sysOperationComplete(sysopId, false, null);
    }
    
}
