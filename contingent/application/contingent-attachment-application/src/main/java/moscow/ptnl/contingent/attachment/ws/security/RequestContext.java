package moscow.ptnl.contingent.attachment.ws.security;

import moscow.ptnl.contingent.domain.security.Principal;
import ru.mos.emias.system.v1.usercontext.UserContext;

import java.util.UUID;

/**
 * Инкапсулирует данные о запросе.
 *
 * @author m.kachalov
 */
public class RequestContext {
    
    private final UUID requestId; //произвольный уникальтный идентификатор запроса
    private final String methodName; //имя вызываемого метода web-сервиса
    private final UserContext userContext;
    
    public RequestContext(String methodName, UserContext userContext) {
        this.requestId = UUID.randomUUID();
        this.methodName = methodName;
        this.userContext = userContext;
    }
    
    //пакетный доступ, чтобы зря не дергали метод
    String getRequestId() {
        return this.requestId.toString();
    }

    //пакетный доступ, чтобы зря не дергали метод
    String getMethodName() {
        return methodName;
    }
    
    //пакетный доступ, чтобы зря не дергали метод
    UserContext getUserContext() {
        return this.userContext;
    }
    
    //пакетный доступ, чтобы зря не дергали метод
    Principal getPrincipal() {        
        if (userContext == null) {
            return null;
        }
        Principal principal = new Principal(userContext.getUserName());
        principal.setIpAddress(userContext.getHostIp());
        principal.setUserRoleId((userContext.getUserRoleId() != 0) ? userContext.getUserRoleId() : null);
        if (userContext.getUserRights() != null) {
            principal.getAccessRights().addAll(userContext.getUserRights().getUserRightIds());
        }
        principal.setJobInfoId(userContext.getJobExecutionId()); //FIXME - это правильно?
        //TODO не понятно как заполнять
        //principal.setAccountId(Long.MIN_VALUE);
        //principal.setFullName(fullName);
        //principal.setLpuId(Long.MIN_VALUE);
        return principal;
    }
    
}
