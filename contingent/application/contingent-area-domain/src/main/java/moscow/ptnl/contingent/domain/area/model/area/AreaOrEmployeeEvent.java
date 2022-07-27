package moscow.ptnl.contingent.domain.area.model.area;

import java.math.BigInteger;
import java.time.LocalDateTime;

public interface AreaOrEmployeeEvent {

    BigInteger getObjType();

    BigInteger getObjectId();

    LocalDateTime getUpdateDate();

    String getUserLogin();

    BigInteger getUserJobId();

    //Area fields
    String getDescriptionOld();

    String getDescriptionNew();

    String getNumberOld();

    String getNumberNew();

    String getCreateDateOld();

    String getCreateDateNew();

    String getArchivedOld();

    String getArchivedNew();

    //Medical employee fields
    BigInteger getMedicalEmployeeJobId();

    String getStartDateOld();

    String getStartDateNew();

    String getEndDateOld();

    String getEndDateNew();

    String getTempDutyStartDateOld();

    String getTempDutyStartDateNew();

    String getIsErrorOld();

    String getIsErrorNew();

    String getReplacementOld();

    String getReplacementNew();
}
