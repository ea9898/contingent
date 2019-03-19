package moscow.ptnl.ws.security;

import ru.mos.emias.system.v1.usercontext.UserContext;

public class UserContextHolder {

    private static ThreadLocal<UserContext> userContexts = new ThreadLocal<>();

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
}
