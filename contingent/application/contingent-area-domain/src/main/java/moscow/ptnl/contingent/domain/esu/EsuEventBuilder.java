package moscow.ptnl.contingent.domain.esu;

import moscow.ptnl.util.XMLUtil;
import org.springframework.integration.support.MessageBuilder;
import org.springframework.messaging.Message;

// Заглушка для формирования сообщения для сохранения в БД топика и отправки в ЕСУ
public class EsuEventBuilder {

    private final EsuOutput esuOutput;

    private EsuEventBuilder(String topic) {
        this.esuOutput = new EsuOutput();
        this.esuOutput.setTopic(topic);
    }

    public EsuOutput build() {
        return new EsuOutput();
    }

    public static EsuEventBuilder withTopic(String topic) {
        EsuEventBuilder builder = new EsuEventBuilder(topic);
        return builder;
    }

    public EsuEventBuilder setMessage(Object o){
        String xmlString = XMLUtil.convertEventObjectToMessage(o, o.getClass());
        esuOutput.setMessage(xmlString);
        return this;
    }

    public Message<EsuOutput> buildMessage() {
        return MessageBuilder
                .withPayload(build())
                .setHeader("TOPIC", esuOutput.getTopic())
                .build();
    }

}
