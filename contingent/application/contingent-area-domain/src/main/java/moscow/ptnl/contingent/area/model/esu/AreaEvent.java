package moscow.ptnl.contingent.area.model.esu;

import java.util.ArrayList;
import java.util.List;
import moscow.ptnl.contingent.area.entity.area.Area;
import moscow.ptnl.contingent.area.entity.area.AreaToAreaType;

public abstract class AreaEvent {

    private final OperationType operationType;
    private final Area area;
    private final Area oldArea;
    private final List<AreaToAreaType> addPrimaryAreaTypes;
    private final List<AreaToAreaType> delPrimaryAreaTypes;

    AreaEvent(OperationType operationType, Area area, Area oldArea, List<AreaToAreaType> addPrimaryAreaTypes,
            List<AreaToAreaType> delPrimaryAreaTypes) {
        this.operationType = operationType;
        this.area = area;
        this.oldArea = oldArea;
        this.addPrimaryAreaTypes = addPrimaryAreaTypes;
        this.delPrimaryAreaTypes = delPrimaryAreaTypes;
    }

    AreaEvent(OperationType operationType, Area area, List<AreaToAreaType> addPrimaryAreaTypes) {
        this(operationType, area, null, addPrimaryAreaTypes, null);
    }

    public OperationType getOperationType() {
        return operationType;
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
