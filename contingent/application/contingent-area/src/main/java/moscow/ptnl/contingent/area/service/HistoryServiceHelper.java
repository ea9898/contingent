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
        historyService.write(UserContextHolder.getRequestUUID(), UserContextHolder.getPrincipal(), oldObject, newObject, cls);
    }
}
