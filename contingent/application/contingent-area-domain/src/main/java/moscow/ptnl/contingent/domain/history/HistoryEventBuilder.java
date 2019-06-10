package moscow.ptnl.contingent.domain.history;

import java.time.LocalDateTime;
import java.util.HashSet;
import java.util.Set;
import moscow.ptnl.contingent.security.Principal;
import org.springframework.integration.support.MessageBuilder;
import org.springframework.messaging.Message;

/**
 * Вспомогательный класс для удобного формирования события, публикуемого в
 * дальнейшем в историю изменений.
 * 
 * @author m.kachalov
 */
public class HistoryEventBuilder {
    
    private final HistoryEvent event;
    private final Set<HistoryEventValue> values;    
    
    private HistoryEventBuilder(Class<?> objectType, String objectId) {
        this.event = new HistoryEvent();
        this.event.setObjectType(objectType.getSimpleName());
        this.event.setObjectId(objectId);
        this.event.setChangeDate(LocalDateTime.now());        
        this.values = new HashSet<>();        
    }
    
    /**
     * Инициализация билдера с заполнением обязательных полей события.
     * 
     * @param entityType тип логируемой сущности
     * @param entityId уникальный идентификатор сущности
     * @return 
     */
    public static HistoryEventBuilder withEntity(Class<?> entityType, String entityId) {
        HistoryEventBuilder builder = new HistoryEventBuilder(entityType, entityId);
        return builder;
    }
    
    public HistoryEventBuilder setAccountId(Long accountId) {
        event.setAccountId(accountId);
        return this;
    }
    
    public HistoryEventBuilder setUserLogin(String userLogin) {
        event.setUserLogin(userLogin);
        return this;
    }
    
    public HistoryEventBuilder setJobInfoId(Long jobInfoId) {
        event.setJobInfoId(jobInfoId);
        return this;
    }

    public HistoryEventBuilder setLpuId(Long lpuId) {
        event.setLpuId(lpuId);
        return this;
    }

    public HistoryEventBuilder setSourceType(String sourceType) {
        event.setSourceType(sourceType);
        return this;
    }

    public HistoryEventBuilder setServiceName(ServiceName serviceName) {
        event.setServiceName(serviceName);
        return this;
    }

    public HistoryEventBuilder setMethodName(String methodName) {
        event.setMethodName(methodName);
        return this;
    }

    public HistoryEventBuilder setEventId(Long eventId) {
        event.setEventId(eventId);
        return this;
    }

    public HistoryEventBuilder setNotificationId(Long notificationId) {
        event.setNotificationId(notificationId);
        return this;
    }

    public HistoryEventBuilder setUserRoleId(Long userRoleId) {
        event.setUserRoleId(userRoleId);
        return this;
    }

    public HistoryEventBuilder addValue(String columnName, String oldValue, String newValue) {
        this.values.add(new HistoryEventValue(event, columnName, oldValue, newValue));
        return this;
    }
    
    public HistoryEventBuilder setPrincipal(Principal principal) {
        event.setAccountId(principal.getAccountId());
        event.setJobInfoId(principal.getJobInfoId());
        event.setLpuId(principal.getLpuId());
        event.setUserLogin(principal.getUsername());
        event.setUserRoleId(principal.getUserRoleId());
        return this;
    }
    
    public HistoryEvent build() {        
        event.setValues(values);
        return event;
    }

    public Message<HistoryEvent> buildMessage() {
        return MessageBuilder.withPayload(build()).build();
    }
    
    
}
