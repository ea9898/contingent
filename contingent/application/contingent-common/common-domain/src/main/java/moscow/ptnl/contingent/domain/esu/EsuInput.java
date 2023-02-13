package moscow.ptnl.contingent.domain.esu;

import moscow.ptnl.contingent.domain.esu.converter.StatusConverter;

import jakarta.persistence.Column;
import jakarta.persistence.Convert;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.SequenceGenerator;
import jakarta.persistence.Table;
import jakarta.validation.constraints.Size;
import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.Objects;

@Entity
@Table(name = "ESU_INPUT")
@SequenceGenerator(name = "ESU_INPUT_SEQ_ID", sequenceName = "ESU_INPUT_SEQ_ID", allocationSize=1)
public class EsuInput implements Serializable {

    private static final long serialVersionUID = -8690088440702064644L;

    @Id
    @GeneratedValue(strategy= GenerationType.SEQUENCE, generator="ESU_INPUT_SEQ_ID")
    @Column(name = "ID", unique = true, nullable = false)
    private Long id;

    @Size(max = 255)
    @Column(name = "ESU_ID")
    private String esuId;

    @Column(name = "ESU_OFFSET")
    private Long esuOffset;

    @Column(name = "PARTITION")
    private Integer partition;

    @Column(name = "RECEIVED_TIME", nullable = false)
    private LocalDateTime receivedTime;

    @Column(name = "UPDATE_TIME")
    private LocalDateTime updateTime;

    @Size(max = 255)
    @Column(name = "TOPIC", nullable = false)
    private String topic;

    @Column(name = "MESSAGE", nullable = false)
    private String message;

    @Column(name = "ERROR_MESSAGE")
    private String errorMessage;

    @Column(name = "STATUS")
    @Convert(converter = StatusConverter.class)
    private EsuStatusType status;

    @Column(name = "EVENT_ID")
    private String eventId;

    @Column(name = "HOST")
    private String host;

    public EsuInput() {
    }

    public EsuInput(String esuId, Long esuOffset, Integer partition, String topic, String message, LocalDateTime receivedTime, LocalDateTime updateTime) {
        this.esuId = esuId;
        this.esuOffset = esuOffset;
        this.partition = partition;
        this.receivedTime = receivedTime;
        this.updateTime = updateTime;
        this.topic = topic;
        this.message = message;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getEsuId() {
        return esuId;
    }

    public void setEsuId(String esuId) {
        this.esuId = esuId;
    }

    public Long getEsuOffset() {
        return esuOffset;
    }

    public void setEsuOffset(Long esuOffset) {
        this.esuOffset = esuOffset;
    }

    public Integer getPartition() {
        return partition;
    }

    public void setPartition(Integer partition) {
        this.partition = partition;
    }

    public LocalDateTime getReceivedTime() {
        return receivedTime;
    }

    public void setReceivedTime(LocalDateTime receivedTime) {
        this.receivedTime = receivedTime;
    }

    public LocalDateTime getUpdateTime() {
        return updateTime;
    }

    public void setUpdateTime(LocalDateTime updateTime) {
        this.updateTime = updateTime;
    }

    public String getErrorMessage() {
        return errorMessage;
    }

    public void setErrorMessage(String errorMessage) {
        this.errorMessage = errorMessage;
    }

    public String getTopic() {
        return topic;
    }

    public void setTopic(String topic) {
        this.topic = topic;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }

    public EsuStatusType getStatus() {
        return status;
    }

    public void setStatus(EsuStatusType status) {
        this.status = status;
    }

    public String getEventId() {
        return eventId;
    }

    public void setEventId(String eventId) {
        this.eventId = eventId;
    }

    public String getHost() {
        return host;
    }

    public void setHost(String host) {
        this.host = host;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj != null && obj instanceof EsuInput) {
            return ((EsuInput) obj).getId().equals(this.id);
        }
        return false;
    }

    @Override
    public int hashCode() {        
        return Objects.hashCode(this.id);
    }
}
