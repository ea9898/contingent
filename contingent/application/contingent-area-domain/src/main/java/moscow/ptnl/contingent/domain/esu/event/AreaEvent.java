package moscow.ptnl.contingent.domain.esu.event;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import moscow.ptnl.contingent.domain.area.entity.area.Area;
import moscow.ptnl.contingent.domain.area.entity.area.AreaToAreaType;

public abstract class AreaEvent implements ESUEvent {

    private final LocalDateTime operationDate;
    private final Area area;
    private final Area oldArea;
    //Todo если не требуется, удалить
    private final List<AreaToAreaType> addPrimaryAreaTypes;
    //Todo если не требуется, удалить
    private final List<AreaToAreaType> delPrimaryAreaTypes;

    AreaEvent(LocalDateTime operationDate, Area area, Area oldArea,
              List<AreaToAreaType> addPrimaryAreaTypes, List<AreaToAreaType> delPrimaryAreaTypes) {
        this.operationDate = operationDate;
        this.area = area;
        this.oldArea = oldArea;
        this.addPrimaryAreaTypes = addPrimaryAreaTypes;
        this.delPrimaryAreaTypes = delPrimaryAreaTypes;
    }

    AreaEvent(LocalDateTime operationDate, Area area, List<AreaToAreaType> addPrimaryAreaTypes) {
        this(operationDate, area, null, addPrimaryAreaTypes, null);
    }

    @Override
    public LocalDateTime getOperationDate() {
        return operationDate;
    }

    public Area getArea() {
        return area;
    }

    public Area getOldArea() {
        return oldArea;
    }

    public List<AreaToAreaType> getAddPrimaryAreaTypes() {
        return addPrimaryAreaTypes == null ? new ArrayList<>() : addPrimaryAreaTypes;
    }

    public List<AreaToAreaType> getDelPrimaryAreaTypes() {
        return delPrimaryAreaTypes == null ? new ArrayList<>() : delPrimaryAreaTypes;
    }
}
