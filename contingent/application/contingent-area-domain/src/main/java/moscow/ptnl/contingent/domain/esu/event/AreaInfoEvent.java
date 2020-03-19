package moscow.ptnl.contingent.domain.esu.event;

import moscow.ptnl.contingent.domain.area.entity.area.Area;

import java.time.LocalDateTime;

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
