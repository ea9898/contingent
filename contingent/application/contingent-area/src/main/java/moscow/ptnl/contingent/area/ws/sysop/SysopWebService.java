package moscow.ptnl.contingent.area.ws.sysop;

import moscow.ptnl.contingent.area.entity.sysop.Sysop;
import moscow.ptnl.contingent.area.entity.sysop.SysopMsg;
import moscow.ptnl.contingent.area.entity.sysop.SysopMsgParam;
import moscow.ptnl.contingent.area.service.SysopService;
import moscow.ptnl.contingent.area.transform.SoapExceptionMapper;
import moscow.ptnl.contingent.area.transform.SysopMapper;
import moscow.ptnl.contingent.area.transform.SysopMsgMapper;
import moscow.ptnl.contingent.area.transform.SysopMsgParamMapper;
import moscow.ptnl.contingent.area.ws.BaseService;

import moscow.ptnl.contingent.error.ContingentException;
import moscow.ptnl.contingent.repository.sysop.SysopMsgParamRepository;
import moscow.ptnl.contingent.repository.sysop.SysopMsgRepository;
import ru.mos.emias.contingent2.sysop.types.ErrorMessage;
import ru.mos.emias.contingent2.sysop.types.ErrorMessageCollection;
import ru.mos.emias.contingent2.sysop.types.ErrorMessageTypes;
import ru.mos.emias.contingent2.sysop.types.GetOperationStatusRequest;
import ru.mos.emias.contingent2.sysop.types.GetOperationStatusResponse;
import ru.mos.emias.contingent2.sysop.types.OperationCompletenessPercentage;
import ru.mos.emias.contingent2.sysop.types.OperationExecutionStatus;
import ru.mos.emias.contingent2.sysop.v1.Fault;
import ru.mos.emias.contingent2.sysop.v1.SysopPT;

import org.apache.cxf.annotations.SchemaValidation;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import moscow.ptnl.contingent.domain.security.annotation.EMIASSecured;

/**
 *
 * @author sorlov
 */
@Service(SysopWebService.SERVICE_NAME)
@Transactional(propagation = Propagation.REQUIRED, rollbackFor = Exception.class)
@SchemaValidation(type = SchemaValidation.SchemaValidationType.BOTH)
public class SysopWebService extends BaseService implements SysopPT {

    private final static Logger LOG = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass().getName());

    public static final String SERVICE_NAME = "SYSOP";

    @Autowired
    private SysopService sysopService;

    @Autowired
    private SysopMapper sysopMapper;

    @Autowired
    private SysopMsgParamMapper sysopMsgParamMapper;

    @Autowired
    private SysopMsgRepository sysopMsgRepository;

    @Autowired
    private SysopMsgMapper sysopMsgMapper;

    @Autowired
    private SysopMsgParamRepository sysopMsgParamRepository;

    @Override @EMIASSecured
    public GetOperationStatusResponse getOperationStatus(GetOperationStatusRequest body) throws Fault {
        try {
            GetOperationStatusResponse response = new GetOperationStatusResponse();
            Sysop sysop = sysopService.getOperationStatus(body.getOperationId());

            OperationCompletenessPercentage operationCompletenessPercentage = new OperationCompletenessPercentage();
            operationCompletenessPercentage.setCompletenessStatus(sysop.getProgress() != null ? sysop.getProgress().shortValue(): 0);

            OperationExecutionStatus operationExecutionStatus = sysopMapper.entityToDtoTransform(sysop);

            List<SysopMsg> sysopMsgList = sysopMsgRepository.getSysopMsgBySysop(sysop);

            operationExecutionStatus.setMessages(mapErrorMessages(sysopMsgList));

            response.setOperationExecutionStatus(operationExecutionStatus);

            return response;
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    private ErrorMessageCollection mapErrorMessages(List<SysopMsg> sysopMsgList) {

        Map<SysopMsg, List<SysopMsgParam>> sysopMsgParamsMap =
                sysopMsgParamRepository.getSysopMsgParamsBySysopMsgList(sysopMsgList);

        ErrorMessageCollection errorMessageCollection = new ErrorMessageCollection();

        Map<SysopMsg, List<SysopMsg>> parentSysopMsgMap = sysopMsgRepository.getSysopMsgChildsMap(sysopMsgList);

        sysopMsgList.forEach(msg -> {
            ErrorMessage errorMessage = new ErrorMessage();
            errorMessage.setType(Enum.valueOf(ErrorMessageTypes.class, msg.getType()));
            errorMessage.setCode(msg.getCode());
            errorMessage.setMessage(msg.getMessage());

            ErrorMessage.Parameters parameters = new ErrorMessage.Parameters();
            parameters.getParameters().addAll(
                    sysopMsgParamsMap.get(msg).stream().map(sysopMsgParamMapper::entityToDtoTransform).collect(Collectors.toList())
            );

            if (parentSysopMsgMap.get(msg) != null && !parentSysopMsgMap.get(msg).isEmpty()) {
                errorMessage.setMessages(mapErrorMessages(new ArrayList<>(msg.getChildMessages())));
            }

            errorMessage.setParameters(parameters);
            errorMessageCollection.getMessages().add(errorMessage);
            }
        );

        return errorMessageCollection;
    }

    private Fault mapException(Exception ex) {
        if (!(ex instanceof ContingentException)) {
            LOG.error(ex.getMessage(), ex);
        }
        //TODO Нужно как то решить вопрос с разными типами Fault.
        //TODO Возможно вынести <wsdl:message name="fault"> в общий wsdl файл
        ru.mos.emias.contingent2.area.Fault f = SoapExceptionMapper.map(ex);
        return new Fault(f.getMessage(), f.getFaultInfo(), f.getCause());
    }
}
