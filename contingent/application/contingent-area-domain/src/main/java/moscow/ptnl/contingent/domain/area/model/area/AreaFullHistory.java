package moscow.ptnl.contingent.domain.area.model.area;

import java.math.BigInteger;
import java.time.LocalDateTime;
import java.util.List;

public class AreaFullHistory {

    private long areaId;

    private List<AreaEvent> areaEvents;

    private List<MedicalEmployeeEvent> medicalEmployeeEvents;

    public long getAreaId() { return areaId; }

    public void setAreaId(long areaId) { this.areaId = areaId;}

    public List<AreaEvent> getAreaEvents() {
        return areaEvents;
    }

    public void setAreaEvents(List<AreaEvent> areaEvents) {
        this.areaEvents = areaEvents;
    }

    public List<MedicalEmployeeEvent> getMedicalEmployeeEvents() {
        return medicalEmployeeEvents;
    }

    public void setMedicalEmployeeEvents(List<MedicalEmployeeEvent> medicalEmployeeEvents) {
        this.medicalEmployeeEvents = medicalEmployeeEvents;
    }

    public interface Event {

        LocalDateTime getUpdateDate();

        String getUserLogin();

        BigInteger getUserJobId();
    }

    public interface AreaEvent extends Event {

        String getDescriptionOld();

        String getDescriptionNew();

        String getNumberOld();

        String getNumberNew();

        String getCreateDateOld();

        String getCreateDateNew();

        String getArchivedOld();

        String getArchivedNew();
    }

    public interface MedicalEmployeeEvent extends Event {

        BigInteger getMedicalEmployeeJobId();

        String getStartDateOld();

        String getStartDateNew();

        String getEndDateOld();

        String getEndDateNew();

        String getTempDutyStartDateOld();

        String getTempDutyStartDateNew();

        String getIsErrorOld();

        String getIsErrorNew();

        String getIsReplacementOld();

        String getIsReplacementNew();
    }
}
