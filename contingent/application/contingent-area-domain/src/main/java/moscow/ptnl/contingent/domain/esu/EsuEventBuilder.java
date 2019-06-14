package moscow.ptnl.contingent.domain.esu;

import org.springframework.integration.support.MessageBuilder;
import org.springframework.messaging.Message;

// Заглушка для формирования сообщения для сохранения в БД топика и отправки в ЕСУ
public class EsuEventBuilder {
    
    public static final String TOPIC_HEADER_NAME = "ESU_TOPIC";

    private final String topicName;
    private Object eventObject;

    private EsuEventBuilder(String topic) {
        this.topicName = topic;
    }

    public static EsuEventBuilder withTopic(String topic) {
        EsuEventBuilder builder = new EsuEventBuilder(topic);
        return builder;
    }

    public EsuEventBuilder setEventObject(Object object){        
        this.eventObject = object;
        return this;
    }

    public Message<Object> buildMessage() {
        return MessageBuilder
                .withPayload(this.eventObject)
                .setHeader(TOPIC_HEADER_NAME, this.topicName)
                .build();
    }

}
