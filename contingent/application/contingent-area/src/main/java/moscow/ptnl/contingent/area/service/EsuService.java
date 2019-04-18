package moscow.ptnl.contingent.area.service;

import moscow.ptnl.contingent.area.model.esu.AreaEvent;

import java.time.LocalDateTime;

public interface EsuService {

    boolean saveAndPublishToESU(AreaEvent event);

    void periodicalPublishUnsuccessMessagesToESU(LocalDateTime olderThan);
}
