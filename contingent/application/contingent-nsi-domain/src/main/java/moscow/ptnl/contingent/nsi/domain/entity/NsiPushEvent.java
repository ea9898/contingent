package moscow.ptnl.contingent.nsi.domain.entity;



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

import moscow.ptnl.contingent.domain.converter.BooleanIntegerConverter;

@Entity
@Table(name = "NSI_PUSH_LOG")
@SequenceGenerator(name = "SEQ_NSI_PUSH_LOG", sequenceName = "SEQ_NSI_PUSH_LOG", allocationSize=1)
public class NsiPushEvent implements Serializable {

    private static final long serialVersionUID = -1047920239396677745L;

    @Id
    @GeneratedValue(strategy= GenerationType.SEQUENCE, generator="SEQ_NSI_PUSH_LOG")
    @Column(name = "id", unique = true, nullable = false)
    private Long id;

    @Size(max = 100)
    @Column(name = "in_type")
    private String inType;

    @Column(name = "received_time")
    private LocalDateTime receivedTime;

    @Column(name = "input_message", nullable = false)
    private String inputMessage;

    @Column(name = "error")
    @Convert(converter = BooleanIntegerConverter.class)
    private Boolean error;

    @Column(name = "error_message")
    private String errorMessage;

    public NsiPushEvent() {
    }

    public NsiPushEvent(String inType, LocalDateTime receivedTime, String inputMessage, Boolean error) {
        this.error = error;
        this.inType = inType;
        this.receivedTime = receivedTime;
        this.inputMessage = inputMessage;
    }

    public Long getId() {
        return id;
    }

    public String getInType() {
        return inType;
    }

    public void setInType(String inType) {
        this.inType = inType;
    }

    public LocalDateTime getReceivedTime() {
        return receivedTime;
    }

    public void setReceivedTime(LocalDateTime receivedTime) {
        this.receivedTime = receivedTime;
    }

    public String getInputMessage() {
        return inputMessage;
    }

    public void setInputMessage(String inputMessage) {
        this.inputMessage = inputMessage;
    }

    public Boolean getError() {
        return error;
    }

    public void setError(Boolean error) {
        this.error = error;
    }

    public String getErrorMessage() {
        return errorMessage;
    }

    public void setErrorMessage(String errorMessage) {
        this.errorMessage = errorMessage;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof NsiPushEvent)) return false;

        NsiPushEvent that = (NsiPushEvent) o;

        if (id != null ? !id.equals(that.id) : that.id != null) return false;
        if (inType != null ? !inType.equals(that.inType) : that.inType != null) return false;
        if (receivedTime != null ? !receivedTime.equals(that.receivedTime) : that.receivedTime != null) return false;
        return inputMessage != null ? inputMessage.equals(that.inputMessage) : that.inputMessage == null;
    }

    @Override
    public int hashCode() {
        int result = id != null ? id.hashCode() : 0;
        result = 31 * result + (inType != null ? inType.hashCode() : 0);
        result = 31 * result + (receivedTime != null ? receivedTime.hashCode() : 0);
        result = 31 * result + (inputMessage != null ? inputMessage.hashCode() : 0);
        return result;
    }
}
