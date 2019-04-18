package moscow.ptnl.contingent.area.service;

import moscow.ptnl.contingent.area.entity.area.MuProfile;
import moscow.ptnl.contingent.area.error.ContingentException;

import java.time.LocalDate;
import java.util.List;

public interface AreaServiceInternal {

    List<MuProfile> getProfileMU(Long muId) throws ContingentException;

    void setProfileMU(Long muId, String muTypeId, List<String> areaTypesAdd, List<String> areaTypesDel) throws ContingentException;

    Long createPrimaryArea(long moId, long muId, String name, Integer number, String areaTypeCode,
                           Integer ageMin, Integer ageMax, Integer ageMinM, Integer ageMaxM, Integer ageMinW, Integer ageMaxW,
                           boolean autoAssignForAttachment, Boolean attachByMedicalReason, String description) throws ContingentException;

    Long createDependentArea(long moId, long muId, String name, Integer number, String areaTypeCode, List<String> primaryAreaTypeCodes,
                             Integer ageMin, Integer ageMax, Integer ageMinM, Integer ageMaxM, Integer ageMinW, Integer ageMaxW,
                             boolean autoAssignForAttachment, String description) throws ContingentException;

    void updatePrimaryArea(long areaId, String name, Integer number,
                           Integer ageMin, Integer ageMax, Integer ageMinM, Integer ageMaxM, Integer ageMinW, Integer ageMaxW,
                           boolean autoAssignForAttachment, Boolean attachByMedicalReason, String description) throws ContingentException;

    void updateDependentArea(long areaId, Long muId, String name, Integer number, List<String> primaryAreaTypeCodesAdd,
                             List<String> primaryAreaTypeCodesDel,
                             Integer ageMin, Integer ageMax, Integer ageMinM, Integer ageMaxM, Integer ageMinW, Integer ageMaxW,
                             boolean autoAssignForAttachment, String description) throws ContingentException;

    Long createOrder(String number, LocalDate date, String ouz, String name) throws ContingentException;
}
