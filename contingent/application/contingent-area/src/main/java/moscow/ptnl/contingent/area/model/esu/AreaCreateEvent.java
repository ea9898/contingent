package moscow.ptnl.contingent.area.model.esu;

import moscow.ptnl.contingent.area.entity.area.Area;
import moscow.ptnl.contingent.area.entity.area.AreaToAreaType;

import java.util.List;

public class AreaCreateEvent extends AreaEvent {

    public AreaCreateEvent(Area area, List<AreaToAreaType> primaryAreaTypes) {
        super(OperationType.CREATE, area, primaryAreaTypes);
    }
}
