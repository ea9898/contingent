package moscow.ptnl.contingent.area.service;

import moscow.ptnl.contingent.service.history.HistoryService;
import moscow.ptnl.ws.security.UserContextHolder;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class HistoryServiceHelper {

    @Autowired
    private HistoryService historyService;

    public <T> T clone(T object) {
        return historyService.clone(object);
    }

    public <T> void sendHistory(T oldObject, T newObject, Class<T> cls) {
        /*
        По стектрейсу раньше определялось имя метода сервиса для логирования в историю
        
        List<StackTraceElement> stElements = Arrays.asList(Thread.currentThread().getStackTrace());

        //TODO исправить хардкод
        String methodName = stElements.stream().filter(ste ->
                ste.getFileName() != null && (ste.getFileName().startsWith("AreaServiceImpl")
                        || ste.getFileName().startsWith("AreaServiceInternalImpl"))).findFirst().get().getMethodName();
        */
        
        historyService.write(
                UserContextHolder.getRequestId(), 
                UserContextHolder.getMethodName(), 
                UserContextHolder.getPrincipal(), 
                oldObject, newObject, cls);
    }
}
