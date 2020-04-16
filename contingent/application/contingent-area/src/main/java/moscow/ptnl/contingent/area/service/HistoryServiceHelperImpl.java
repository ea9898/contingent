package moscow.ptnl.contingent.area.service;

import moscow.ptnl.contingent.domain.area.HistoryServiceHelper;
import moscow.ptnl.contingent.domain.area.HistoryService;
import moscow.ptnl.contingent.security.UserContextHolder;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class HistoryServiceHelperImpl implements HistoryServiceHelper {

    @Autowired
    private HistoryService historyService;

    public <T> T clone(T object) {
        return historyService.clone(object);
    }

    public <T> void sendHistory(T oldObject, T newObject, Class<T> cls) {

        historyService.write(
                UserContextHolder.getRequestId(), 
                UserContextHolder.getMethodName(), 
                UserContextHolder.getPrincipal(), 
                oldObject, newObject, cls);
    }
}
