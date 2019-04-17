package moscow.ptnl.contingent.area.service;

import moscow.ptnl.contingent.area.model.esu.AreaEvent;

import java.util.Date;

public interface EsuService {

    boolean saveAndPublishToESU(AreaEvent event);

    void periodicalPublishUnsuccessMessagesToESU(Date olderThan);
}
