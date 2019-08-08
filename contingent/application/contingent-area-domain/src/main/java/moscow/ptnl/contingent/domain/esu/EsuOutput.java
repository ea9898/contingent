package moscow.ptnl.contingent.domain.esu;

import javax.persistence.Cacheable;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.validation.constraints.Size;
import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.Objects;
import javax.persistence.Convert;
import moscow.ptnl.contingent.domain.esu.converter.StatusConverter;

@Entity
@Table(name = "ESU_OUTPUT")
@SequenceGenerator(name = "ESU_OUTPUT_SEQ_ID", sequenceName = "ESU_OUTPUT_SEQ_ID", allocationSize=1)
@Cacheable
public class EsuOutput implements Serializable {

    private static final long serialVersionUID = 6195350410114428881L;

    @Id
    @GeneratedValue(strategy= GenerationType.SEQUENCE, generator="ESU_OUTPUT_SEQ_ID")
    @Column(name = "ID", unique = true, nullable = false)
    private Long id;

    @Size(max = 255)
    @Column(name = "ESU_ID")
    private String esuId;

    @Column(name = "\"OFFSET\"")
    private Long offset;

    @Column(name = "PARTITION")
    private Integer partition;

    @Column(name = "SENT_TIME", nullable = false)
    private LocalDateTime sentTime;

    @Size(max = 255)
    @Column(name = "TOPIC", nullable = false)
    private String topic;

    @Column(name = "MESSAGE", nullable = false)
    private String message;

    @Column(name = "STATUS", nullable = false)
    @Convert(converter = StatusConverter.class)
    private EsuStatusType status = EsuStatusType.UNSUCCESS;

    @Column(name = "METHOD_NAME", nullable = true)
    private String method;

    @Column(name = "HOST")
    private String host;

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

    public Long getOffset() {
        return offset;
    }

    public void setOffset(Long offset) {
        this.offset = offset;
    }

    public Integer getPartition() {
        return partition;
    }

    public void setPartition(Integer partition) {
        this.partition = partition;
    }

    public LocalDateTime getSentTime() {
        return sentTime;
    }

    public void setSentTime(LocalDateTime sentTime) {
        this.sentTime = sentTime;
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

    public String getMethod() {
        return method;
    }

    public void setMethod(String method) {
        this.method = method;
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
        if (obj != null && obj instanceof EsuOutput) {
            return ((EsuOutput) obj).getId().equals(this.id);
        }
        return false;
    }

    @Override
    public int hashCode() {        
        return Objects.hashCode(this.id);
    }
}
