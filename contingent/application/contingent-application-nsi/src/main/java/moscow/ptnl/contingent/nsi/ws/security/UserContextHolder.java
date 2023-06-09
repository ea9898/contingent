package moscow.ptnl.contingent.nsi.ws.security;

import ru.mos.emias.system.v1.usercontext.UserContext;


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
    
    public static UserContext getUserContext() {
        RequestContext context = getContext();
        if (context == null) {
            return null;
        }
        return context.getUserContext();
    }

    public static void setContext(RequestContext context) {
        if (context != null) {
            REQUEST_CONTEXT.set(context);
        } else {
            REQUEST_CONTEXT.remove();;
        }
    }
}
