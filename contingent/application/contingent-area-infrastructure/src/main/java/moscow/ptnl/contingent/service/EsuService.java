package moscow.ptnl.contingent.service;


import java.time.LocalDateTime;
import moscow.ptnl.contingent.area.model.esu.ESUEvent;

public interface EsuService {

    boolean saveAndPublishToESU(ESUEvent event);

    void periodicalPublishUnsuccessMessagesToESU(LocalDateTime olderThan);
}
