package moscow.ptnl.contingent.domain.esu.event;

import moscow.ptnl.contingent.area.entity.area.Area;

import java.time.LocalDateTime;
import java.util.Set;

public class CreateCloseAttachmentEvent extends AreaEvent {

    private final OperationType operationType;
    private final Set<Long> primaryAreaIds;

    public CreateCloseAttachmentEvent(OperationType operationType, Area dependentArea, Set<Long> primaryAreaIds) {
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
