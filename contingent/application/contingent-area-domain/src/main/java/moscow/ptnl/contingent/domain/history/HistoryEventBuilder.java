package moscow.ptnl.contingent.domain.history;

import java.time.LocalDateTime;
import java.util.HashSet;
import java.util.Set;

import moscow.ptnl.contingent.domain.area.entity.AddressAllocationOrders;
import moscow.ptnl.contingent.domain.area.entity.Area;
import moscow.ptnl.contingent.domain.area.entity.AreaAddress;
import moscow.ptnl.contingent.domain.area.entity.AreaMedicalEmployees;
import moscow.ptnl.contingent.domain.area.entity.AreaMuService;
import moscow.ptnl.contingent.domain.area.entity.MoAddress;
import moscow.ptnl.contingent.nsi.domain.area.EventTypes;
import moscow.ptnl.contingent.security.Principal;
import org.springframework.integration.support.MessageBuilder;
import org.springframework.messaging.Message;

import jakarta.persistence.Table;

/**
 * Вспомогательный класс для удобного формирования события, публикуемого в
 * дальнейшем в историю изменений.
 * 
 * @author m.kachalov
 */
public class HistoryEventBuilder {
    
    private final HistoryEvent event;
    private final Set<HistoryEventValue> values;    
    
    private HistoryEventBuilder(Class<?> objectType, String objectId, String methodName) {
        this.event = new HistoryEvent();
        this.event.setObjectType(getObjectType(objectType, methodName));
        this.event.setObjectId(objectId);
        this.event.setChangeDate(LocalDateTime.now());        
        this.values = new HashSet<>();        
    }

    private String getTableName(Class<?> objectType) {
        return objectType.getAnnotation(Table.class) != null ? objectType.getAnnotation(Table.class).name() : objectType.getSimpleName();
    }

    private String getObjectType(Class<?> objectType, String methodName) {
        if (objectType.isAssignableFrom(Area.class)) {
            return "AREA";
        }
        else if (objectType.isAssignableFrom(AreaMedicalEmployees.class)) {
            return "AREA_EMPLOYEE";
        }
        else if (objectType.isAssignableFrom(AreaAddress.class)) {
            return "AREA_ADDRESS";
        }
        else if (objectType.isAssignableFrom(MoAddress.class)) {
            return "MO_ADDRESS";
        }
        else if (objectType.isAssignableFrom(AreaMuService.class)) {
            return "SERVICE_MU";
        }
        else if (objectType.isAssignableFrom(AddressAllocationOrders.class)) {
            return "ADDRESS_ALLOCATION_ORDERS";
        }
        return "";
    }

    /**
     * Инициализация билдера с заполнением обязательных полей события.
     * 
     * @param entityType тип логируемой сущности
     * @param entityId уникальный идентификатор сущности
     * @return 
     */
    public static HistoryEventBuilder withEntity(Class<?> entityType, String entityId, String methodName) {
        HistoryEventBuilder builder = new HistoryEventBuilder(entityType, entityId, methodName);
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

    public HistoryEventBuilder setEventTypeId(Long eventTypeId) {
        event.setEventType(eventTypeId == null ? null :
                new EventTypes() {{ setId(eventTypeId); }});
        return this;
    }

    public HistoryEventBuilder setOperationLinkId(Long operationLinkId) {
        event.setOperationLinkId(operationLinkId);
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
