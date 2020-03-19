package moscow.ptnl.contingent.attachment.ws.v1;

import moscow.ptnl.contingent.attachment.service.AttachmentServiceInternal;
import moscow.ptnl.contingent.attachment.transform.SoapExceptionMapper;
import moscow.ptnl.contingent.security.annotation.EMIASSecured;
import moscow.ptnl.contingent.error.ContingentException;
import moscow.ptnl.metrics.Metrics;
import org.apache.cxf.annotations.SchemaValidation;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
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
@SchemaValidation(type = SchemaValidation.SchemaValidationType.BOTH)
public class AttachmentServiceImpl implements AttachmentPT {

    private final static Logger LOG = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass().getName());

    public static final String SERVICE_NAME = "ATTACHMENT_V1";

    @Autowired
    private AttachmentServiceInternal attachmentServiceInternal;

    @Override @EMIASSecured @Metrics
    public InitiatePersonalAreaAttachmentResponse initiatePersonalAreaAttachment(InitiatePersonalAreaAttachmentRequest body) throws Fault {
        try {
            attachmentServiceInternal.initiatePersonalAreaAttachment(body.getPatientEmiasId(), body.getOperationDate(),
                    body.getCreateAttachment() != null ? body.getCreateAttachment().getJobId() : null,
                    body.getCloseAttachment() != null ? body.getCloseAttachment().getJobId() : null);
            return new InitiatePersonalAreaAttachmentResponse();
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }



    private Fault mapException(Exception ex) {
        if (!(ex instanceof ContingentException)) {
            LOG.error(ex.getMessage(), ex);
        }
        return SoapExceptionMapper.map(ex);
    }
}
