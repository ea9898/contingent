package moscow.ptnl.contingent.area.model.esu;

import moscow.ptnl.contingent.area.entity.area.Area;
import moscow.ptnl.contingent.area.entity.area.AreaToAreaType;

import java.util.List;

public abstract class AreaEvent {

    private final OperationType operationType;
    private final Area area;
    private final List<AreaToAreaType> primaryAreaTypes;

    AreaEvent(OperationType operationType, Area area, List<AreaToAreaType> primaryAreaTypes) {
        this.operationType = operationType;
        this.area = area;
        this.primaryAreaTypes = primaryAreaTypes;
    }

    public OperationType getOperationType() {
        return operationType;
    }

    public Area getArea() {
        return area;
    }

    public List<AreaToAreaType> getPrimaryAreaTypes() {
        return primaryAreaTypes;
    }
}
