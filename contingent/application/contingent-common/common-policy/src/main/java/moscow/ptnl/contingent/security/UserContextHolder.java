package moscow.ptnl.contingent.security;

import moscow.ptnl.contingent.security.Principal;
import moscow.ptnl.contingent.security.RequestContext;


public class UserContextHolder {

    private static final ThreadLocal<RequestContext> REQUEST_CONTEXT = new ThreadLocal<>();
       
    public static RequestContext getContext() {
        return REQUEST_CONTEXT.get();
    }
    
    public static String getRequestId() {
        RequestContext context = getContext();
        if (context == null) {
            return null;
        }
        return context.getRequestId();
    }
    
    public static String getMethodName() {
        RequestContext context = getContext();
        if (context == null) {
            return null;
        }
        return context.getMethodName();
    }

    public static int getContractVersion() {
        RequestContext context = getContext();
        if (context == null || context.getContractVersion() == null) {
            return 1;
        }
        try {
            return Integer.parseInt(context.getContractVersion().substring(context.getContractVersion().indexOf('v') + 1));
        } catch (Exception ex) {
            return 1;
        }
    }

    public static UserContext getUserContext() {
        RequestContext context = getContext();
        if (context == null) {
            return null;
        }
        return context.getUserContext();
    }
    
    public static Principal getPrincipal() {
        RequestContext context = getContext();
        if (context == null) {
            return null;
        }
        return context.getPrincipal();
    }
    
    public static void setContext(RequestContext context) {
        if (context != null) {
            REQUEST_CONTEXT.set(context);
        } else {
            REQUEST_CONTEXT.remove();;
        }
    }
}
