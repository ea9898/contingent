package moscow.ptnl.contingent.nsi.ws.security;

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
}
