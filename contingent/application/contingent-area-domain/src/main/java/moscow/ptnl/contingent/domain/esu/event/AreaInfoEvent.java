package moscow.ptnl.contingent.area.model.esu;

import moscow.ptnl.contingent.area.entity.area.Area;
import moscow.ptnl.contingent.area.entity.area.AreaToAreaType;

import java.time.LocalDateTime;
import java.util.List;

public class AreaInfoEvent extends AreaEvent {

    private final String operationType;

    public AreaInfoEvent(String operationType, Area area) {
        super(LocalDateTime.now(), area, null);
        this.operationType = operationType;
    }

    public String getOperationType() {
        return operationType;
    }
}
