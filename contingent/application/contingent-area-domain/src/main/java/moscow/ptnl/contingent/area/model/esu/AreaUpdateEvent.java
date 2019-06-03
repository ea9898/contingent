package moscow.ptnl.contingent.area.model.esu;

import java.util.List;
import moscow.ptnl.contingent.area.entity.area.Area;
import moscow.ptnl.contingent.area.entity.area.AreaToAreaType;

public class AreaUpdateEvent extends AreaEvent {

    public AreaUpdateEvent(Area area, Area oldArea, List<AreaToAreaType> addPrimaryAreaTypes, List<AreaToAreaType> delPrimaryAreaTypes) {
        super(OperationType.UPDATE, area, oldArea, addPrimaryAreaTypes, delPrimaryAreaTypes);
    }
}
