package moscow.ptnl.ws.security;

import moscow.ptnl.contingent.security.Principal;
import ru.mos.emias.system.v1.usercontext.UserContext;

public class UserContextHolder {

    private static ThreadLocal<UserContext> userContexts = new ThreadLocal<>();

    private static String requestUUIDs;

    public static String getRequestUUID() {
        return requestUUIDs;
    }

    public static void setRequestUUID(String requestUUID) {
        if (requestUUID != null) {
            requestUUIDs = requestUUID;
        } else {
            requestUUIDs = null;
        }
    }

    public static UserContext getContext() {
        return userContexts.get();
    }

    public static void setContext(UserContext userContext) {
        if (userContext != null) {
            userContexts.set(userContext);
        } else {
            userContexts.remove();
        }
    }
    
    public static Principal getPrincipal() {
        UserContext userContext = getContext();
        if (userContext == null) {
            return null;
        }
        Principal principal = new Principal(userContext.getUserName());
        principal.setIpAddress(userContext.getHostIp());
        principal.setUserRoleId((userContext.getUserRoleId() != 0) ? userContext.getUserRoleId() : null);
        principal.setJobInfoId(userContext.getJobExecutionId()); //FIXME - это правильно?
        //TODO не понятно как заполнять
        //principal.setAccountId(Long.MIN_VALUE);
        //principal.setFullName(fullName);
        //principal.setLpuId(Long.MIN_VALUE);
        return principal;
    }
}
