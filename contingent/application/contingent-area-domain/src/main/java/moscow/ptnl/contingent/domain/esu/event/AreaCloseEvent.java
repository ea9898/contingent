package moscow.ptnl.contingent.domain.esu.event;

import moscow.ptnl.contingent.area.entity.area.Area;

import java.time.LocalDateTime;

@Deprecated
public class AreaCloseEvent extends AreaEvent {

    public AreaCloseEvent(Area area) {
        super(LocalDateTime.now(), area, null);
    }
}