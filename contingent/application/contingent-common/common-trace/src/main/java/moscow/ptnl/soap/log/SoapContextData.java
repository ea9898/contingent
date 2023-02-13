package moscow.ptnl.soap.log;

import java.time.LocalDateTime;

public class SoapContextData {

    private String uuid;
    private String method;
    private String request;
    private String response;
    private LocalDateTime time;

    public SoapContextData() {
    }

    public SoapContextData(String method, String request, String response) {
        this.method = method;
        this.request = request;
        this.response = response;
    }

    public String getUuid() {
        return uuid;
    }

    public void setUuid(String uuid) {
        this.uuid = uuid;
    }

    public String getMethod() {
        return method;
    }

    public void setMethod(String method) {
        this.method = method;
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

    public LocalDateTime getTime() {
        return time == null ? LocalDateTime.MIN : time;
    }

    public void setTime(LocalDateTime time) {
        this.time = time;
    }
}
