package moscow.ptnl.contingent.nsi.ws;

import moscow.ptnl.contingent.domain.nsi.entity.NsiPushEvent;
import moscow.ptnl.contingent.repository.nsi.NsiPushEventCRUDRepository;
import moscow.ptnl.contingent.nsi.pushaccepter.PushAccepter;
import org.apache.cxf.annotations.SchemaValidation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import ru.mos.emias.pushaccepterproduct.pushaccepterservice.v1.PushaccepterServicePortType;
import ru.mos.emias.pushaccepterproduct.pushaccepterservice.v1.types.ChangeElement;
import ru.mos.emias.pushaccepterproduct.pushaccepterservice.v1.types.ResponseElement;

import java.time.LocalDateTime;

@Service(NsiWebServiceImpl.SERVICE_NAME)
//@Transactional(propagation = Propagation.REQUIRED, rollbackFor = Exception.class)
@SchemaValidation(type = SchemaValidation.SchemaValidationType.BOTH)
public class NsiWebServiceImpl implements PushaccepterServicePortType {

    public static final String SERVICE_NAME = "NSI_V1";

    @Autowired
    PushAccepter pushAccepter;

    @Autowired
    NsiPushEventCRUDRepository nsiPushEventCRUDRepository;

    @Override
    public ResponseElement get(ChangeElement getChangeElement) {
        /* TODO:
            все что касается маппинга в объект и сохранения данных в таблицу нужно вынести в отдельный поток с транзакционностью,
            в случае ошибки обработки входного сообщения нам нужно записать в БД ошибку обработки входного сообщения.
            В случае успешной обработки входного сообщения, объект нужно передавать по шине SI по новому
            асинхронному каналу NsiEventChannel (заглушка есть)
            Со стороны приложения NSI нужно реализовать отправку в канал NsiEventChannel.
         */
        NsiPushEvent event = new NsiPushEvent(getChangeElement.getIntype(), LocalDateTime.now(), getChangeElement.getIn());
        nsiPushEventCRUDRepository.save(event);
        return pushAccepter.get(getChangeElement);
    }
}
