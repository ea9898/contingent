package moscow.ptnl.contingent.nsi.ws;

import org.apache.cxf.annotations.SchemaValidation;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import ru.mos.emias.pushaccepterproduct.pushaccepterservice.v1.GetService;
import ru.mos.emias.pushaccepterproduct.pushaccepterservice.v1.PushaccepterServicePortType;
import ru.mos.emias.pushaccepterproduct.pushaccepterservice.v1.types.ChangeElement;
import ru.mos.emias.pushaccepterproduct.pushaccepterservice.v1.types.ResponseElement;

import javax.transaction.Transactional;

@Service(NsiWebServiceImpl.SERVICE_NAME)
//@Transactional(propagation = Propagation.REQUIRED, rollbackFor = Exception.class)
@SchemaValidation(type = SchemaValidation.SchemaValidationType.BOTH)
public class NsiWebServiceImpl implements PushaccepterServicePortType {

    public static final String SERVICE_NAME = "NSI_V1";

    @Override
    public ResponseElement get(ChangeElement getChangeElement) {
        return null;
    }
}
