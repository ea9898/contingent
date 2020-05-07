package moscow.ptnl.contingent.security;

import javax.xml.bind.annotation.XmlElement;
import java.util.ArrayList;
import java.util.List;

public class UserContext {

    private String systemName;

    private String userName;

    private long userRoleId;

    private Boolean isUserRoleSystemWide;

    private List<Long> userRights = new ArrayList<>();

    private long jobExecutionId;

    private String hostIp;

    private String hostName;

    public String getSystemName() {
        return systemName;
    }

    public void setSystemName(String systemName) {
        this.systemName = systemName;
    }

    public String getUserName() {
        return userName;
    }

    public void setUserName(String userName) {
        this.userName = userName;
    }

    public long getUserRoleId() {
        return userRoleId;
    }

    public void setUserRoleId(long userRoleId) {
        this.userRoleId = userRoleId;
    }

    public Boolean getUserRoleSystemWide() { return isUserRoleSystemWide; }

    public void setUserRoleSystemWide(Boolean userRoleSystemWide) { isUserRoleSystemWide = userRoleSystemWide; }

    public List<Long> getUserRights() {
        return userRights;
    }

    public void setUserRights(List<Long> userRights) {
        this.userRights = userRights;
    }

    public long getJobExecutionId() {
        return jobExecutionId;
    }

    public void setJobExecutionId(long jobExecutionId) {
        this.jobExecutionId = jobExecutionId;
    }

    public String getHostIp() {
        return hostIp;
    }

    public void setHostIp(String hostIp) {
        this.hostIp = hostIp;
    }

    public String getHostName() {
        return hostName;
    }

    public void setHostName(String hostName) {
        this.hostName = hostName;
    }
}
