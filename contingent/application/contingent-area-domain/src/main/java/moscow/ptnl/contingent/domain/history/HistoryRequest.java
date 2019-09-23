package moscow.ptnl.contingent.domain.history;

import moscow.ptnl.soap.log.SoapContextData;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;
import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.Objects;

/**
 *
 * @author sorlov
 */
@Entity
@SequenceGenerator(
        name = HistoryRequest.SEQUENCE_GENERATOR_NAME,
        sequenceName = HistoryRequest.SEQUENCE_GENERATOR_NAME,
        allocationSize = 1
)
@Table(name = "JL_HISTORY_REQUESTS")
public class HistoryRequest implements Serializable {

    static final String SEQUENCE_GENERATOR_NAME = "JL_HIST_REQ_SEQ_ID";

    @Id
    @Column(name = "ID")
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = HistoryRequest.SEQUENCE_GENERATOR_NAME)
    private Long id;

    @Column(name = "METHOD_NAME")
    @Size(max = 255)
    private String methodName;

    @Column(name = "REQUEST")
    @NotNull
    private String request;

    @Column(name = "RESPONSE")
    private String response;

    @Column(name = "CALL_TIME")
    private LocalDateTime callTime;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getMethodName() {
        return methodName;
    }

    public void setMethodName(String methodName) {
        this.methodName = methodName;
    }

    public String getRequest() {
        return request;
    }

    public void setRequest(String request) {
        this.request = request;
    }

    public String getResponse() {
        return response;
    }

    public void setResponse(String response) {
        this.response = response;
    }

    public LocalDateTime getCallTime() {
        return callTime;
    }

    public void setCallTime(LocalDateTime callTime) {
        this.callTime = callTime;
    }

    @Override
    public boolean equals(Object object) {
        if (object == null)
            return false;
        if (object == this)
            return true;
        if (!(object instanceof HistoryRequest))
            return false;
        return Objects.equals(((HistoryRequest) object).getId(), this.id);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(this.id);
    }

    public static HistoryRequest build(SoapContextData data) {
        HistoryRequest request = new HistoryRequest();
        request.setMethodName(data.getMethod());
        request.setRequest(data.getRequest());
        request.setResponse(data.getResponse());
        request.setCallTime(data.getTime());

        return request;
    }
}
