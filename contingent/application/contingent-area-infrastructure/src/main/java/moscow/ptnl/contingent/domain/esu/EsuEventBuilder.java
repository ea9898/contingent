package moscow.ptnl.contingent.domain.esu;


import moscow.ptnl.contingent.security.Principal;
import org.springframework.integration.support.MessageBuilder;
import org.springframework.messaging.Message;

// Заглушка для формирования сообщения для сохранения в БД топика и отправки в ЕСУ
public class EsuEventBuilder {

    public EsuEventBuilder() {
    }

    public EsuOutput build() {
        return new EsuOutput();
    }

    public static EsuEventBuilder withTableAndObject() {
        EsuEventBuilder builder = new EsuEventBuilder();
        return builder;
    }

    public EsuEventBuilder addTopic(Object o){
        return this;
    }

    public EsuEventBuilder setPrincipal(Principal principal) {
//        event.setAccountId(principal.getAccountId());
//        event.setJobInfoId(principal.getJobInfoId());
//        event.setLpuId(principal.getLpuId());
//        event.setUserLogin(principal.getUsername());
//        event.setUserRoleId(principal.getUserRoleId());
        return this;
    }

    public Message<EsuOutput> buildMessage() {
        return MessageBuilder.withPayload(build()).build();
    }

}
