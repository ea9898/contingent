package moscow.ptnl.contingent.attachment.ws.v1;

import org.apache.cxf.annotations.SchemaValidation;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import ru.mos.emias.contingent2.attachment.v1.AttachmentPT;
import ru.mos.emias.contingent2.attachment.v1.Fault;
import ru.mos.emias.contingent2.attachment.v1.types.InitiatePersonalAreaAttachmentRequest;
import ru.mos.emias.contingent2.attachment.v1.types.InitiatePersonalAreaAttachmentResponse;

import java.lang.invoke.MethodHandles;

/**
 *
 * @author mkachalov
 */
@Service(AttachmentServiceImpl.SERVICE_NAME)
@Transactional(propagation = Propagation.REQUIRED, rollbackFor = Exception.class)
@SchemaValidation(type = SchemaValidation.SchemaValidationType.BOTH)
public class AttachmentServiceImpl implements AttachmentPT {

    private final static Logger LOG = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass().getName());

    public static final String SERVICE_NAME = "V1";


    @Override
    public InitiatePersonalAreaAttachmentResponse initiatePersonalAreaAttachment(InitiatePersonalAreaAttachmentRequest body) throws Fault {
        return new InitiatePersonalAreaAttachmentResponse();
    }
}
