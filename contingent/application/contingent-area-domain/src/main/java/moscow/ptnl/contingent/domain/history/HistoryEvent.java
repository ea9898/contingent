package moscow.ptnl.contingent.domain.history;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;
import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Convert;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToMany;
import jakarta.persistence.SequenceGenerator;
import jakarta.persistence.Table;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;
import moscow.ptnl.contingent.nsi.domain.annotation.MapToNsi;
import moscow.ptnl.contingent.nsi.domain.area.EventTypes;
import moscow.ptnl.contingent.nsi.domain.area.PositionSupp;

/**
 *
 * @author mkachalov
 */
@Entity
@SequenceGenerator(
        name = HistoryEvent.SEQUENCE_GENERATOR_NAME, 
        sequenceName = HistoryEvent.SEQUENCE_GENERATOR_NAME, 
        allocationSize = 1
)
@Table(name = "JL_HISTORY")
public class HistoryEvent implements Serializable {
    
    public static final String SEQUENCE_GENERATOR_NAME = "JL_JRN_SEQ_ID";
    
    @Id @Column(name = "ID") 
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = HistoryEvent.SEQUENCE_GENERATOR_NAME)
    private Long id;
    
    @Column(name = "OBJECT_TYPE") @NotNull    
    private String objectType;
    
    @Column(name = "OBJECT_ID") @NotNull
    private String objectId;

    @JoinColumn(name = "EVENT_TYPE_ID")
    @ManyToOne(fetch = FetchType.LAZY)
    private EventTypes eventTypeId;

    @Column(name = "OPERATION_LINK_ID")
    private Long operationLinkId;

    @Column(name = "CHANGE_DATE") @NotNull
    private LocalDateTime changeDate;

    @Column(name = "USER_LOGIN")
    private String userLogin;
    
    @Column(name = "JOB_INFO_ID")
    private Long jobInfoId;
    
    @Column(name = "SERVICE_NAME")
    @Convert(converter = ServiceName.Converter.class)
    private ServiceName serviceName;
    
    @Column(name = "METHOD_NAME")
    private String methodName;

    @Column(name = "USER_ROLE_ID")
    private Long userRoleId;

    @Column(name = "jl_history_requests_id")
    private String requestId;
    
    @OneToMany(mappedBy = "event", cascade = CascadeType.ALL)
    private Set<HistoryEventValue> values;
    
    public HistoryEvent(){}

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getObjectType() {
        return objectType;
    }

    public void setObjectType(String objectType) {
        this.objectType = objectType;
    }

    public String getObjectId() {
        return objectId;
    }

    public void setObjectId(String objectId) {
        this.objectId = objectId;
    }

    public LocalDateTime getChangeDate() {
        return changeDate;
    }

    public void setChangeDate(LocalDateTime changeDate) {
        this.changeDate = changeDate;
    }

    public String getUserLogin() {
        return userLogin;
    }

    public void setUserLogin(String userLogin) {
        this.userLogin = userLogin;
    }

    public Long getJobInfoId() {
        return jobInfoId;
    }

    public void setJobInfoId(Long jobInfoId) {
        this.jobInfoId = jobInfoId;
    }

    public ServiceName getServiceName() {
        return serviceName;
    }

    public void setServiceName(ServiceName serviceName) {
        this.serviceName = serviceName;
    }

    public String getMethodName() {
        return methodName;
    }

    public void setMethodName(String methodName) {
        this.methodName = methodName;
    }

    public Long getUserRoleId() {
        return userRoleId;
    }

    public void setUserRoleId(Long userRoleId) {
        this.userRoleId = userRoleId;
    }

    public String getRequestId() {
        return requestId;
    }

    public void setRequestId(String requestId) {
        this.requestId = requestId;
    }

    public EventTypes getEventTypeId() {
        return eventTypeId;
    }

    public void setEventTypeId(EventTypes eventTypeId) {
        this.eventTypeId = eventTypeId;
    }

    public Long getOperationLinkId() {
        return operationLinkId;
    }

    public void setOperationLinkId(Long operationLinkId) {
        this.operationLinkId = operationLinkId;
    }

    public Set<HistoryEventValue> getValues() {
        if (values == null) {
            values = new HashSet<>();
        }
        return values;
    }

    public void setValues(Set<HistoryEventValue> values) {
        this.values = values;
    }
    
    @Override
    public boolean equals(Object object) {
        if (object == null)
            return false;
        if (object == this)
            return true;
        if (!(object instanceof HistoryEvent))
            return false;
        return ((HistoryEvent) object).getId().equals(this.id);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(this.id);
    }
    
}
