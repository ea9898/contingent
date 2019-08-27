package moscow.ptnl.contingent.area.endpoint;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.integration.annotation.MessageEndpoint;
import org.springframework.integration.annotation.ServiceActivator;
import org.springframework.messaging.Message;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import static moscow.ptnl.contingent.area.configuration.EventChannelsConfiguration.ESU_EVENT_CHANNEL_NAME;
import moscow.ptnl.contingent.domain.esu.EsuEventBuilder;
import moscow.ptnl.contingent.service.esu.EsuService;
import moscow.ptnl.util.Strings;
import org.springframework.beans.factory.annotation.Autowired;

/**
 * Точка получения событий из канала ESU_EVENT_CHANNEL_NAME (событий для ЕСУ).
 * 
 * @author m.kachalov
 */
@Service
@Transactional(propagation = Propagation.REQUIRED, rollbackFor = Throwable.class)
@MessageEndpoint
public class ESUEventEndpoint {

    private static final Logger LOG = LoggerFactory.getLogger(ESUEventEndpoint.class);

    @Autowired
    private EsuService esuService;

    @ServiceActivator(inputChannel = ESU_EVENT_CHANNEL_NAME)
    public void esuOutputConsumer(Message<Object> msg) {
        LOG.debug("input message: ");
        String topicName = (String) msg.getHeaders().get(EsuEventBuilder.TOPIC_HEADER_NAME);
        if (Strings.isNullOrEmpty(topicName)) {
            LOG.error("в сообщении не определен ЕСУ-топик");
            return;
        }
        try {
            Object event = msg.getPayload();
            if (!esuService.saveAndPublishToESU(topicName, event)) {
                LOG.warn("не удалось опубликовать сообщение в ЕСУ");
            }
        } catch (Exception e) {
            LOG.error("ошибка публикации в ЕСУ-топик", e);
        }
    }
}
