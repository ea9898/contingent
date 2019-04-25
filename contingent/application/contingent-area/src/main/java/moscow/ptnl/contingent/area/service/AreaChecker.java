package moscow.ptnl.contingent.area.service;

import moscow.ptnl.contingent.area.entity.area.Area;
import moscow.ptnl.contingent.area.entity.area.MuProfile;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypes;
import moscow.ptnl.contingent.area.entity.nsi.MUProfileTemplates;
import moscow.ptnl.contingent.area.error.AreaErrorReason;
import moscow.ptnl.contingent.area.error.Validation;
import moscow.ptnl.contingent.area.error.ValidationParameter;
import moscow.ptnl.contingent.area.repository.area.AreaCRUDRepository;
import moscow.ptnl.contingent.area.repository.area.AreaRepository;
import moscow.ptnl.contingent.area.repository.area.MuProfileRepository;
import moscow.ptnl.contingent.area.repository.nsi.AreaTypesCRUDRepository;
import moscow.ptnl.contingent.area.repository.nsi.MuProfileTemplatesRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.time.LocalDate;
import java.time.Month;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

@Component
public class AreaChecker {

    @Autowired
    private AreaTypesCRUDRepository areaTypesCRUDRepository;

    @Autowired
    private MuProfileTemplatesRepository muProfileTemplatesRepository;

    @Autowired
    private MuProfileRepository muProfileRepository;

    @Autowired
    private AreaRepository areaRepository;

    @Autowired
    private AreaCRUDRepository areaCRUDRepository;

    public void checkAreaTypesExist(List<Long> areaTypes, Validation validation, String parameterCode) {
        areaTypes.forEach(a -> {
            Optional<AreaTypes> areaType = areaTypesCRUDRepository.findById(a);

            if (!areaType.isPresent() || Boolean.TRUE.equals(areaType.get().getArchived())) {
                validation.error(AreaErrorReason.AREA_TYPE_NOT_FOUND, new ValidationParameter(parameterCode, a));
            }
        });
    }

    public void checkMuProfileChangePossible(int muTypeId, Long areaType, Validation validation, String parameterCode) {
        MUProfileTemplates template = muProfileTemplatesRepository.findMuProfileTemplate(muTypeId, areaType);

        if (template == null || !Boolean.TRUE.equals(template.getAvailableToCreate())) {
            validation.error(AreaErrorReason.CANT_CHANGE_AREA_TYPE, new ValidationParameter(parameterCode, areaType));
        }
    }

    public void checkAreaTypeAgeSetups(AreaTypes areaType, Integer ageMin, Integer ageMax,
                                       Integer ageMinM, Integer ageMaxM, Integer ageMinW, Integer ageMaxW, Validation validation) {
        if (!checkAgeSetupFilling(ageMin, ageMax, areaType.getAgeMin(), areaType.getAgeMax()) ||
                !checkAgeSetupFilling(ageMinM, ageMaxM, areaType.getAgeMMin(), areaType.getAgeMMax()) ||
                !checkAgeSetupFilling(ageMinW, ageMaxW, areaType.getAgeWMin(), areaType.getAgeWMax())) {
            validation.error(AreaErrorReason.INCORRECT_AREA_AGE_SETUPS);
            return;
        }
        checkAgeSetupRange(ageMin, ageMax, areaType.getAgeMin(), areaType.getAgeMax(), "ageMin", "ageMax", validation);
        checkAgeSetupRange(ageMinM, ageMaxM, areaType.getAgeMMin(), areaType.getAgeMMax(), "ageMinM", "ageMaxM", validation);
        checkAgeSetupRange(ageMinW, ageMaxW, areaType.getAgeWMin(), areaType.getAgeWMax(), "ageMinW", "ageMaxW", validation);
    }

    private boolean checkAgeSetupFilling(Integer ageMin, Integer ageMax, Integer ageMinAreaType, Integer ageMaxAreaType) {
        return (ageMin == null || ageMinAreaType != null) && (ageMax == null || ageMaxAreaType != null);
    }

    private void checkAgeSetupRange(Integer ageMin, Integer ageMax, Integer ageMinAreaType, Integer ageMaxAreaType,
                                       String paramMinCode, String paramMaxCode, Validation validation) {
        if (!(ageMin == null || ageMin >= ageMinAreaType) && (ageMax == null || ageMax <= ageMaxAreaType)) {
            validation.error(AreaErrorReason.AREA_AGE_SETUP_EXCEEDED,
                    new ValidationParameter(paramMinCode, ageMin), new ValidationParameter(paramMaxCode, ageMax),
                    new ValidationParameter(paramMinCode, ageMinAreaType), new ValidationParameter(paramMaxCode, ageMaxAreaType));
        }
    }

    public Map<Long, AreaTypes> checkAndGetPrimaryAreaTypesInMU(long muId, List<Long> primaryAreaTypeCodes, Validation validation) {
        List<MuProfile> muProfiles = muProfileRepository.getMuProfilesByMuId(muId);
        Map<Long, AreaTypes> primaryAreaTypes = muProfiles.stream()
                .filter(p -> p.getAreaType() != null)
                .collect(Collectors.toMap(p -> p.getAreaType().getCode(), MuProfile::getAreaType));

        primaryAreaTypeCodes.forEach(c -> {
            if (!primaryAreaTypes.keySet().contains(c)) {
                validation.error(AreaErrorReason.MU_PROFILE_HAS_NO_AREA_TYPE, new ValidationParameter("primaryAreaTypeCode", c));
            }
        });
        return primaryAreaTypes;
    }

    public void checkPrimaryAreasInMU(long muId, List<Long> primaryAreaTypeCodes, Validation validation) {
        StringBuilder primaryAreaTypesMissing = new StringBuilder();
        List<Area> areas = areaRepository.findAreas(null, muId, primaryAreaTypeCodes, null, true);

        primaryAreaTypeCodes.forEach(c -> {
            if (areas.stream().noneMatch(a -> a.getAreaType() != null && Objects.equals(c, a.getAreaType().getCode()))) {
                primaryAreaTypesMissing.append(c).append(", ");
            }
        });
        if (primaryAreaTypesMissing.length() > 0) {
            validation.error(AreaErrorReason.NO_PRIMARY_AREA, new ValidationParameter("primaryAreaTypeCode",
                    primaryAreaTypesMissing.substring(0, primaryAreaTypesMissing.length() - 2)));
        }
    }

    public Area checkAndGetArea(long areaId, Validation validation) {
        Area area = areaCRUDRepository.findById(areaId).orElse(null);

        if (area == null) {
            validation.error(AreaErrorReason.AREA_NOT_FOUND, new ValidationParameter("areaId", areaId));
        }
        else if (area.getArchived()) {
            validation.error(AreaErrorReason.AREA_IS_ARCHIVED, new ValidationParameter("areaId", areaId));
        }
        return area;
    }

    public void checkDateTillToday(LocalDate date, Validation validation) {
        if (date.isBefore(LocalDate.of(1970, Month.JANUARY, 1)) ||
                date.isAfter(LocalDate.now())) {
            validation.error(AreaErrorReason.DATE_IN_INCORRECT_PERIOD);
        }
    }
}
