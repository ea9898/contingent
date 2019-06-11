package moscow.ptnl.contingent.service.esu;


import java.time.LocalDateTime;
import moscow.ptnl.contingent.domain.esu.event.ESUEvent;
import moscow.ptnl.contingent.domain.esu.EsuOutput;


public interface EsuService {

    boolean saveAndPublishToESU(ESUEvent event);

    void periodicalPublishUnsuccessMessagesToESU(LocalDateTime olderThan);
}
