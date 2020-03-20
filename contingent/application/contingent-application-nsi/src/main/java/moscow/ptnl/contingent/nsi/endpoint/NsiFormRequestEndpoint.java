package moscow.ptnl.contingent.nsi.endpoint;

import moscow.ptnl.contingent.nsi.domain.NsiFormConstraint;
import moscow.ptnl.contingent.nsi.domain.NsiFormTablesEnum;
import moscow.ptnl.contingent.nsi.service.NsiFormServiceHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.integration.annotation.MessageEndpoint;
import org.springframework.integration.annotation.ServiceActivator;
import org.springframework.integration.support.MessageBuilder;
import org.springframework.messaging.Message;
import org.springframework.messaging.MessageChannel;
import org.springframework.stereotype.Service;
import org.w3c.dom.Document;
import ru.mos.emias.system.v1.usercontext.UserContext;

import javax.xml.ws.soap.SOAPFaultException;

import java.util.Queue;
import java.util.Set;

import static moscow.ptnl.contingent.nsi.configuration.Constraint.*;

/**
 * Точка получения событий из канала NSI_FORM_REQUEST_CHANNEL_NAME.
 * 
 * @author sorlov
 */
@Service
@MessageEndpoint
public class NsiFormRequestEndpoint {

    private static final Logger LOG = LoggerFactory.getLogger(NsiFormRequestEndpoint.class);

    private static final String NSI_SOAP_NOT_FOUND_MSG = "Объект не найден";

    @Autowired
    @Qualifier(NSI_FORM_CHANNEL_NAME)
    private MessageChannel saveChannel;

    @Autowired
    private NsiFormServiceHelper nsiFormServiceHelper;

    @ServiceActivator(inputChannel = NSI_FORM_REQUEST_CHANNEL_NAME, async = "true")
    public void nsiFormRequestConsumer(Message<Object> msg) {
        String tn = Thread.currentThread().getName();
        LOG.info("{} сообщение: {}", NSI_FORM_REQUEST_CHANNEL_NAME, msg);
        Long globalId = (Long) msg.getPayload();
        Long formId = (Long) msg.getHeaders().get(NsiFormConstraint.FORM_ID_HEADER);
        NsiFormTablesEnum entityType = (NsiFormTablesEnum) msg.getHeaders().get(NsiFormConstraint.ENTITY_TYPE_HEADER);
        UserContext context = (UserContext) msg.getHeaders().get(NsiFormConstraint.USER_CONTEXT);
        Queue<Long> unrecognizedAddresses = (Queue<Long>) msg.getHeaders().get(NsiFormConstraint.UNRECOGNIZED_ADDRESSES);
        Document response;

        try {
            response = nsiFormServiceHelper.searchByGlobalId(formId, globalId, context);
            LOG.debug("Вызов НСИ метода searchByGlobalId [formId = " + formId + " globalId = " + globalId + "]");
        }
        catch (Throwable th) {
            LOG.info("Ошибка при вызове НСИ метода searchByGlobalId [formId = " + formId + " globalId = " + globalId + "]", th);

            if (unrecognizedAddresses != null) {
                unrecognizedAddresses.offer(globalId);
            }
            return;
        }
        //Отправляем дальше на парсинг и сохранение в БД
        saveChannel.send(MessageBuilder
                .withPayload(response)
                .setHeader(NsiFormConstraint.FORM_ID_HEADER, formId)
                .setHeader(NsiFormConstraint.GLOBAL_ID_HEADER, globalId)
                .setHeader(NsiFormConstraint.ENTITY_TYPE_HEADER, entityType)
                .build());
    }
}
