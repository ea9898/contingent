package moscow.ptnl.contingent.area.model.esu;

import moscow.ptnl.contingent.area.entity.area.Area;


public class AreaCloseEvent extends AreaEvent {

    public AreaCloseEvent(Area area) {
        super(OperationType.CLOSE, area, null);
    }
}
