package moscow.ptnl.contingent.area.model.esu;

import moscow.ptnl.contingent.area.entity.area.Area;
import moscow.ptnl.contingent.area.entity.area.AreaToAreaType;

import java.util.List;

public class AreaUpdateEvent extends AreaEvent {

    public AreaUpdateEvent(Area area, List<AreaToAreaType> primaryAreaTypes) {
        super(OperationType.UPDATE, area, primaryAreaTypes);
    }
}
