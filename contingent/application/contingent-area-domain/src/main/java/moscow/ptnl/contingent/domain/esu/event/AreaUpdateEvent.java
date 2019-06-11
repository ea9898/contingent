package moscow.ptnl.contingent.domain.esu.event;

import java.time.LocalDateTime;
import java.util.List;
import moscow.ptnl.contingent.area.entity.area.Area;
import moscow.ptnl.contingent.area.entity.area.AreaToAreaType;

@Deprecated
public class AreaUpdateEvent extends AreaEvent {

    public AreaUpdateEvent(Area area, Area oldArea, List<AreaToAreaType> addPrimaryAreaTypes, List<AreaToAreaType> delPrimaryAreaTypes) {
        super(LocalDateTime.now(), area, oldArea, addPrimaryAreaTypes, delPrimaryAreaTypes);
    }
}
