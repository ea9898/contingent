package moscow.ptnl.contingent.area.ws.nsi;

import moscow.ptnl.contingent.area.ws.BaseService;
import moscow.ptnl.util.Strings;
import org.apache.cxf.annotations.SchemaValidation;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import ru.mos.op.receive_changes.ChangeElement;
import ru.mos.op.receive_changes.ReceiveChangesPort;
import ru.mos.op.receive_changes.ResponseElement;

/**
 * Сервис для получения изменений в НСИ.
 * EMIASUPK-7571
 * 
 * @author m.kachalov
 */
@Service(ReceiveChangesImpl.SERVICE_NAME) 
@SchemaValidation(type = SchemaValidation.SchemaValidationType.BOTH)
public class ReceiveChangesImpl extends BaseService implements ReceiveChangesPort {
    
    private static final Logger LOG = LoggerFactory.getLogger(ReceiveChangesImpl.class);
    
    public static final String SERVICE_NAME = "ReceiveChangesImpl_V1";
    
    private static final String STATUS_OK = "OK";
    private static final String STATUS_FAIL = "FAIL";

    @Override
    public ResponseElement receiveChange(ChangeElement parameters) {
        
        String packageId = null;
        DATA_ACTION dataAction = null;
        String errorMessage = null;
        
        String request = parameters.getIn();
        
        if (!Strings.isNullOrEmpty(request)) {
            try {
                moscow.ptnl.contingent.area.nsi.Package packageObject = jaxbUnmarshall(moscow.ptnl.contingent.area.nsi.Package.class, request);                
                packageId = packageObject.getId();
                if (packageObject.getCatalog() != null && packageObject.getCatalog().getData() != null) {
                    dataAction = DATA_ACTION.valueOf(packageObject.getCatalog().getData().getAction());
                    LOG.debug("request data action: {}", dataAction.name());
                }
            } catch (Exception e) {
                errorMessage = e.getMessage();
                LOG.error("ошибка парсинга xml", e);
            }
        }
        
        
        ResponseElement response = new ResponseElement();
        response.setOut((packageId != null) ? createStatusOK(packageId) : createStatusFAIL(errorMessage));
        return response;
    }
    
    private static String createStatusOK(String packageId) {
        return STATUS_OK + ":" + packageId;
    }
    
    private static String createStatusFAIL(String message) {
        return STATUS_FAIL + ":" + ((message != null) ? message : "error");
    }
    
    public static enum DATA_ACTION {
        ADDED, MODIFIED, DELETED;
    }
    
}
