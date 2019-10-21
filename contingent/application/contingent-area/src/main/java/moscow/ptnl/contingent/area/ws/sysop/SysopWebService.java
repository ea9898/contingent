package moscow.ptnl.contingent.area.ws.sysop;

import moscow.ptnl.contingent.area.service.SysopService;
import moscow.ptnl.contingent.area.transform.SoapExceptionMapper;
import moscow.ptnl.contingent.area.transform.SysopMapper;
import moscow.ptnl.contingent.area.ws.BaseService;

import moscow.ptnl.contingent.error.ContingentException;
import ru.mos.emias.contingent2.sysop.types.GetOperationStatusRequest;
import ru.mos.emias.contingent2.sysop.types.GetOperationStatusResponse;
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

    @Override
    public GetOperationStatusResponse getOperationStatus(GetOperationStatusRequest body) throws Fault {
        try {
            GetOperationStatusResponse response = new GetOperationStatusResponse();
            response.setOperationExecutionStatus((sysopMapper.entityToDtoTransform(sysopService.getOperationStatus(body.getOperationId()))));

            return response;
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
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
