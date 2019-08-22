package moscow.ptnl.contingent.nsi.ws;

import moscow.ptnl.contingent.domain.nsi.entity.NsiPushEvent;
import moscow.ptnl.contingent.repository.nsi.NsiPushEventCRUDRepository;
import moscow.ptnl.contingent.nsi.pushaccepter.PushAccepter;
import org.apache.cxf.annotations.SchemaValidation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import ru.mos.emias.pushaccepterproduct.pushaccepterservice.v1.PushaccepterServicePortType;
import ru.mos.emias.pushaccepterproduct.pushaccepterservice.v1.types.ChangeElement;
import ru.mos.emias.pushaccepterproduct.pushaccepterservice.v1.types.ResponseElement;

import java.time.LocalDateTime;

@Service(NsiWebServiceImpl.SERVICE_NAME)
@SchemaValidation(type = SchemaValidation.SchemaValidationType.BOTH)
@Transactional(propagation = Propagation.REQUIRED, rollbackFor = Exception.class)
public class NsiWebServiceImpl implements PushaccepterServicePortType {

    public static final String SERVICE_NAME = "NSI_V1";

    @Autowired
    PushAccepter pushAccepter;

    @Autowired
    NsiPushEventCRUDRepository nsiPushEventCRUDRepository;

    @Override
    public ResponseElement get(ChangeElement getChangeElement) {
        NsiPushEvent event = new NsiPushEvent(getChangeElement.getIntype(), LocalDateTime.now(), getChangeElement.getIn(), false);
        NsiPushEvent savedEvent = nsiPushEventCRUDRepository.save(event);
        ResponseElement responseElement = pushAccepter.get(getChangeElement, savedEvent.getId());
        if (responseElement.getOut().startsWith("ERROR")) {
            savedEvent.setError(true);
            savedEvent.setErrorMessage(responseElement.getOut());
            nsiPushEventCRUDRepository.save(savedEvent);
        }
        return responseElement;
    }
}
