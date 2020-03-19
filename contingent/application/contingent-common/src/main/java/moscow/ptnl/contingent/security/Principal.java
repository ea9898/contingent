package moscow.ptnl.contingent.security;

import java.util.HashSet;
import java.util.Objects;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 *
 * @author m.kachalov
 */
public class Principal {
    
    private final String username;
    private Long accountId;
    private Long lpuId;
    private Long jobInfoId;
    private Long userRoleId;
    private String ipAddress;
    private String fullName;
    private final Set<Long> accessRights;
    
    public Principal(String username) {
        this.username = username;
        this.accessRights = new HashSet<>();
    }
    
    /** 
     * Получение логина пользователя без наименования системы.
     * @return 
     */
    public String getUsernameWOSys() {
        if (Objects.isNull(getUsername())) {
            return null;
        }
        Pattern r = Pattern.compile("^([\\w]*/)?([\\w]+)$");
        Matcher m = r.matcher(getUsername());
        if (m.find()) {
            return m.group(2);
        }
        return null;
    }

    public Long getAccountId() {
        return accountId;
    }

    public void setAccountId(Long accountId) {
        this.accountId = accountId;
    }

    public Long getLpuId() {
        return lpuId;
    }

    public void setLpuId(Long lpuId) {
        this.lpuId = lpuId;
    }

    public Long getJobInfoId() {
        return jobInfoId;
    }

    public void setJobInfoId(Long jobInfoId) {
        this.jobInfoId = jobInfoId;
    }

    public Long getUserRoleId() {
        return userRoleId;
    }

    public void setUserRoleId(Long userRoleId) {
        this.userRoleId = userRoleId;
    }

    public String getUsername() {
        return username;
    }

    public String getIpAddress() {
        return ipAddress;
    }

    public void setIpAddress(String ipAddress) {
        this.ipAddress = ipAddress;
    }

    public String getFullName() {
        return fullName;
    }

    public void setFullName(String fullName) {
        this.fullName = fullName;
    }

    public Set<Long> getAccessRights() {
        return accessRights;
    }
    
}
