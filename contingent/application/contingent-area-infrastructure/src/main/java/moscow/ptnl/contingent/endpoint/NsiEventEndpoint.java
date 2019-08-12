package moscow.ptnl.contingent.endpoint;

import moscow.ptnl.contingent.domain.esu.EsuEventBuilder;
import moscow.ptnl.contingent.service.esu.EsuService;
import moscow.ptnl.util.Strings;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.integration.annotation.MessageEndpoint;
import org.springframework.integration.annotation.ServiceActivator;
import org.springframework.messaging.Message;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import static moscow.ptnl.contingent.configuration.EventChannelsConfiguration.ESU_EVENT_CHANNEL_NAME;
import static moscow.ptnl.contingent.configuration.EventChannelsConfiguration.NSI_EVENT_CHANNEL_NAME;

/**
 * Точка получения событий из канала ESU_EVENT_CHANNEL_NAME (событий для ЕСУ).
 * 
 * @author m.kachalov
 */
@Service
@Transactional(propagation = Propagation.REQUIRED, rollbackFor = Throwable.class)
@MessageEndpoint
public class NsiEventEndpoint {

    private static final Logger LOG = LoggerFactory.getLogger(NsiEventEndpoint.class);

    @ServiceActivator(inputChannel = NSI_EVENT_CHANNEL_NAME)
    public void nsiOutputConsumer(Message<Object> msg) {
        // TODO реализовать получение и обработку сообщений
    }
}
