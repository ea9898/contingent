package moscow.ptnl.contingent.nsi.ws;

import moscow.ptnl.contingent.infrastructure.service.TransactionRunService;
import moscow.ptnl.contingent.nsi.domain.entity.NsiPushEvent;
import moscow.ptnl.contingent.nsi.repository.NsiPushEventCRUDRepository;
import moscow.ptnl.contingent.nsi.pushaccepter.PushAccepter;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.cxf.annotations.SchemaValidation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import ru.mos.emias.pushaccepterproduct.pushaccepterservice.v1.PushaccepterServicePortType;
import ru.mos.emias.pushaccepterproduct.pushaccepterservice.v1.types.ChangeElement;
import ru.mos.emias.pushaccepterproduct.pushaccepterservice.v1.types.ResponseElement;

import java.time.LocalDateTime;
import java.util.concurrent.ExecutionException;

@Service(NsiWebServiceImpl.SERVICE_NAME)
@SchemaValidation(type = SchemaValidation.SchemaValidationType.BOTH)
@Transactional(propagation = Propagation.REQUIRED, rollbackFor = Exception.class)
public class NsiWebServiceImpl implements PushaccepterServicePortType {

    public static final String SERVICE_NAME = "NSI_V1";

    @Autowired
    private PushAccepter pushAccepter;

    @Autowired
    private TransactionRunService transactionRunService;

    @Autowired
    private NsiPushEventCRUDRepository nsiPushEventCRUDRepository;

    @Override
    public ResponseElement get(ChangeElement getChangeElement) {
        Long id;

        try {
            //Сохраняем в отедльной транзакции, чтобы при асинхронной обработке сообщения в БД точно была сохранена запись
            id = transactionRunService.run(() -> {
                NsiPushEvent event = new NsiPushEvent(getChangeElement.getIntype(), LocalDateTime.now(),
                        getChangeElement.getIn().trim(), null);
                nsiPushEventCRUDRepository.save(event);
                return event.getId();
            }).get();
        }
        catch (InterruptedException | ExecutionException e) {
            ResponseElement responseElement = new ResponseElement();
            responseElement.setOut("FAIL! " + ExceptionUtils.getStackTrace(e));
            return responseElement;
        }
        ResponseElement responseElement = pushAccepter.get(getChangeElement, id);

        if (!responseElement.getOut().startsWith("OK")) {
            NsiPushEvent event = nsiPushEventCRUDRepository.findById(id).get();
            event.setError(true);
            event.setErrorMessage(responseElement.getOut());
        }
        return responseElement;
    }
}
