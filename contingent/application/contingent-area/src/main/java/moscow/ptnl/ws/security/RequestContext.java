/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package moscow.ptnl.ws.security;

import java.util.UUID;
import moscow.ptnl.contingent.domain.security.Principal;
import ru.mos.emias.system.v1.usercontext.UserContext;

/**
 *
 * @author m.kachalov
 */
public class RequestContext {
    
    private final UUID requestId;
    private final UserContext userContext;
    
    public RequestContext(UserContext userContext) {
        this.requestId = UUID.randomUUID();
        this.userContext = userContext;
    }
    
    public String getRequestId() {
        return this.requestId.toString();
    }
    
    public UserContext getUserContext() {
        return this.userContext;
    }
    
    public Principal getPrincipal() {        
        if (userContext == null) {
            return null;
        }
        Principal principal = new Principal(userContext.getUserName());
        principal.setIpAddress(userContext.getHostIp());
        principal.setUserRoleId((userContext.getUserRoleId() != 0) ? userContext.getUserRoleId() : null);
        principal.getAccessRights().addAll(userContext.getUserRights().getUserRightIds());
        principal.setJobInfoId(userContext.getJobExecutionId()); //FIXME - это правильно?
        //TODO не понятно как заполнять
        //principal.setAccountId(Long.MIN_VALUE);
        //principal.setFullName(fullName);
        //principal.setLpuId(Long.MIN_VALUE);
        return principal;
    }
    
}
