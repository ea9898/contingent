package moscow.ptnl.contingent.domain.esu.event;

import moscow.ptnl.contingent.domain.area.entity.area.Area;

import java.time.LocalDateTime;
import java.util.Set;

public class AttachOnAreaChangeEvent extends AreaEvent {

    private final OperationType operationType;
    private final Set<Long> primaryAreaIds;

    public AttachOnAreaChangeEvent(OperationType operationType, Area dependentArea, Set<Long> primaryAreaIds) {
        super(LocalDateTime.now(), dependentArea, null);
        this.operationType = operationType;
        this.primaryAreaIds = primaryAreaIds;
    }

    public OperationType getOperationType() {
        return operationType;
    }

    public Set<Long> getPrimaryAreaIds() {
        return primaryAreaIds;
    }

    public static enum OperationType {

        CREATE, CLOSE
    }
}
