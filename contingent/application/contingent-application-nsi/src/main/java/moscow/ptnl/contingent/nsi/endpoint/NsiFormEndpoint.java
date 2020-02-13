package moscow.ptnl.contingent.nsi.endpoint;

import moscow.ptnl.contingent.nsi.domain.NsiFormConstraint;
import moscow.ptnl.contingent.nsi.domain.NsiFormTablesEnum;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.integration.annotation.MessageEndpoint;
import org.springframework.integration.annotation.ServiceActivator;
import org.springframework.messaging.Message;
import org.springframework.stereotype.Service;
import org.w3c.dom.Document;

import static moscow.ptnl.contingent.nsi.configuration.Constraint.NSI_FORM_CHANNEL_NAME;

/**
 * Точка получения событий из канала NSI_FORM_CHANNEL_NAME
 * 
 * @author sorlov
 */
@Service
@MessageEndpoint
public class NsiFormEndpoint {

    private static final Logger LOG = LoggerFactory.getLogger(NsiFormEndpoint.class);

    @Autowired
    private NsiFormProcessor nsiFormProcessor;

    @ServiceActivator(inputChannel = NSI_FORM_CHANNEL_NAME, async = "true")
    public void nsiFormConsumer(Message<Object> msg) {
        LOG.info("{} сообщение: {}", NSI_FORM_CHANNEL_NAME, msg);
        Document response = (Document) msg.getPayload();
        Long globalId = (Long) msg.getHeaders().get(NsiFormConstraint.GLOBAL_ID_HEADER);
        NsiFormTablesEnum entityType = (NsiFormTablesEnum) msg.getHeaders().get(NsiFormConstraint.ENTITY_TYPE_HEADER);

        try {
            nsiFormProcessor.process(globalId, entityType, response);
        }
        catch (Throwable th) {
            throw new RuntimeException("Ошибка мапинга НСИ адреса на сущность " + entityType, th);
//            LOG.error("Ошибка мапинга НСИ адреса на сущность {}", entityType, th);
        }
    }
}
