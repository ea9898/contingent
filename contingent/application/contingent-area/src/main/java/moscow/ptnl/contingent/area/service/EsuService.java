package moscow.ptnl.contingent.area.service;

import java.io.Serializable;
import java.util.Date;

public interface EsuService {

    boolean saveAndPublishToESU(Serializable data);

    void periodicalPublishUnsuccessMessagesToESU(Date olderThan);
}
