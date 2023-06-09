package moscow.ptnl.contingent.domain.area.model.area;

import java.time.LocalDate;

public class ChangeMedicalEmployee {

    protected long assignmentId;

    protected LocalDate startDate;

    protected LocalDate endDate;

    protected Boolean isTempDuty;

    protected Boolean isError;

    public long getAssignmentId() {
        return assignmentId;
    }

    public void setAssignmentId(long assignmentId) {
        this.assignmentId = assignmentId;
    }

    public LocalDate getStartDate() {
        return startDate;
    }

    public void setStartDate(LocalDate startDate) {
        this.startDate = startDate;
    }

    public LocalDate getEndDate() {
        return endDate;
    }

    public void setEndDate(LocalDate endDate) {
        this.endDate = endDate;
    }

    public Boolean getTempDuty() {
        return isTempDuty;
    }

    public void setTempDuty(Boolean tempDuty) {
        isTempDuty = tempDuty;
    }

    public Boolean isIsError() { return isError; }

    public void setIsError(Boolean error) { isError = error; }
}
