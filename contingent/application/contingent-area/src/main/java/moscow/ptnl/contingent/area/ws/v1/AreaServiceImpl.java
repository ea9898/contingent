package moscow.ptnl.contingent.area.ws.v1;

import moscow.ptnl.contingent.area.ws.BaseService;
import org.apache.cxf.annotations.SchemaValidation;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import ru.gov.emias2.contingent.v1.area.AreaPT;
import ru.gov.emias2.contingent.v1.area.GetProfileMURequest;
import ru.gov.emias2.contingent.v1.area.GetProfileMUResponse;
import ru.gov.emias2.contingent.v1.common.ContingentFault;

/**
 *
 * @author mkachalov
 */
@Service(AreaServiceImpl.SERVICE_NAME) 
@SchemaValidation(type = SchemaValidation.SchemaValidationType.BOTH)
public class AreaServiceImpl extends BaseService implements AreaPT {
    
    public static final String SERVICE_NAME = "V1";

    @Transactional(propagation = Propagation.REQUIRED)
    @Override
    public GetProfileMUResponse getProfileMU(GetProfileMURequest body) throws ContingentFault {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }
}
