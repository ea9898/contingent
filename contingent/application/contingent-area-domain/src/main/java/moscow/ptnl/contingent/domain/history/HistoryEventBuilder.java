package moscow.ptnl.contingent.domain.history;

import java.time.LocalDateTime;
import java.util.HashSet;
import java.util.Set;
import moscow.ptnl.contingent.security.Principal;
import org.springframework.integration.support.MessageBuilder;
import org.springframework.messaging.Message;

import javax.persistence.Table;

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
        this.event.setObjectType(getTableName(objectType));
        this.event.setObjectId(objectId);
        this.event.setChangeDate(LocalDateTime.now());        
        this.values = new HashSet<>();        
    }

    private String getTableName(Class<?> objectType) {
        return objectType.getAnnotation(Table.class) != null ? objectType.getAnnotation(Table.class).name() : objectType.getSimpleName();
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

    public HistoryEventBuilder setUserLogin(String userLogin) {
        event.setUserLogin(userLogin);
        return this;
    }
    
    public HistoryEventBuilder setJobInfoId(Long jobInfoId) {
        event.setJobInfoId(jobInfoId);
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

    public HistoryEventBuilder setUserRoleId(Long userRoleId) {
        event.setUserRoleId(userRoleId);
        return this;
    }

    public HistoryEventBuilder setRequestUUID(String uuid) {
        event.setRequestId(uuid);
        return this;
    }

    public HistoryEventBuilder addValue(String columnName, String oldValue, String newValue, Class<?> objectType) {
        this.values.add(new HistoryEventValue(event, columnName, oldValue, newValue, getTableName(objectType)));
        return this;
    }
    
    public HistoryEventBuilder setPrincipal(Principal principal) {
        event.setJobInfoId(principal.getJobInfoId());
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
