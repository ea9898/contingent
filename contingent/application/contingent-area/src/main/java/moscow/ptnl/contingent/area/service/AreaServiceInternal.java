package moscow.ptnl.contingent.area.service;

import moscow.ptnl.contingent.area.entity.area.MuProfile;
import moscow.ptnl.contingent.area.error.ContingentException;

import java.util.List;

public interface AreaServiceInternal {

    List<MuProfile> getProfileMU(Long muId) throws ContingentException;

    void setProfileMU(Long muId, String muTypeId, List<String> areaTypesAdd, List<String> areaTypesDel) throws ContingentException;
}
