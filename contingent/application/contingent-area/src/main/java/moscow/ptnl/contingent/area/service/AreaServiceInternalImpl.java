package moscow.ptnl.contingent.area.service;

import moscow.ptnl.contingent.area.entity.area.Area;
import moscow.ptnl.contingent.area.entity.area.AreaToAreaType;
import moscow.ptnl.contingent.area.entity.area.MuProfile;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypes;
import moscow.ptnl.contingent.area.error.AreaErrorReason;
import moscow.ptnl.contingent.area.error.Validation;
import moscow.ptnl.contingent.area.error.ValidationParameter;
import moscow.ptnl.contingent.area.repository.area.AreaCRUDRepository;
import moscow.ptnl.contingent.area.repository.area.AreaRepository;
import moscow.ptnl.contingent.area.repository.area.AreaToAreaTypeCRUDRepository;
import moscow.ptnl.contingent.area.repository.area.MuProfileCRUDRepository;
import moscow.ptnl.contingent.area.repository.area.MuProfileRepository;
import moscow.ptnl.contingent.area.repository.nsi.AreaTypesCRUDRepository;
import moscow.ptnl.contingent.area.repository.nsi.MuProfileTemplatesRepository;
import moscow.ptnl.contingent.area.error.ContingentException;

import moscow.ptnl.util.Strings;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

@Component
public class AreaServiceInternalImpl implements AreaServiceInternal {

    @Autowired
    private MuProfileRepository muProfileRepository;

    @Autowired
    private MuProfileCRUDRepository muProfileCRUDRepository;

    @Autowired
    private MuProfileTemplatesRepository muProfileTemplatesRepository;

    @Autowired
    private AreaTypesCRUDRepository areaTypesCRUDRepository;

    @Autowired
    private AreaRepository areaRepository;

    @Autowired
    private AreaCRUDRepository areaCRUDRepository;

    @Autowired
    private AreaToAreaTypeCRUDRepository areaToAreaTypeCRUDRepository;

    @Autowired
    private AreaChecker areaChecker;

    @Override
    public List<MuProfile> getProfileMU(Long muId) throws ContingentException {
        return muProfileRepository.getMuProfilesByMuId(muId);
    }

    @Override
    public void setProfileMU(Long muId, String muTypeId, List<String> areaTypesAdd, List<String> areaTypesDel) throws ContingentException {
        List<String> typesAdd = areaTypesAdd == null ? new ArrayList<>() : areaTypesAdd;
        List<String> typesDel = areaTypesDel == null ? new ArrayList<>() : areaTypesDel;

        Validation validation = new Validation();

        if (typesAdd.isEmpty() && typesDel.isEmpty()) {
            validation.error(AreaErrorReason.NO_INFO_TO_CHANGE);
            throw new ContingentException(validation);
        }
        areaChecker.checkAreaTypesExist(typesAdd, validation, "areaTypesAdd");
        areaChecker.checkAreaTypesExist(typesDel, validation, "areaTypesDel");

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        List<MuProfile> muProfiles = muProfileRepository.getMuProfilesByMuId(muId);
        List<String> attachedAreaTypes = muProfiles.stream()
                .filter(m -> m.getAreaType() != null)
                .map(m -> m.getAreaType().getCode())
                .collect(Collectors.toList());

        typesAdd.forEach(a -> {
            if (attachedAreaTypes.contains(a)) {
                validation.error(AreaErrorReason.MU_PROFILE_EXISTS, new ValidationParameter("muId", muId),
                        new ValidationParameter("areaTypesAdd", a));
                return;
            }
            areaChecker.checkMuProfileChangePossible(Integer.valueOf(muTypeId), a, validation, "areaTypesAdd");
        });
        typesDel.forEach(a -> {
            if (!areaRepository.findAreas(null, muId, a, null, true).isEmpty()) {
                validation.error(AreaErrorReason.CANT_DELETE_AREA_TYPE, new ValidationParameter("areaTypesDel", a));
                return;
            }
            areaChecker.checkMuProfileChangePossible(Integer.valueOf(muTypeId), a, validation, "areaTypesDel");
        });
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        //Удаляем прифили МУ
        List<MuProfile> profilesToDelete = muProfiles.stream()
                .filter(m -> m.getAreaType() != null
                        && typesDel.contains(m.getAreaType().getCode())
                        && !typesAdd.contains(m.getAreaType().getCode()))
                .collect(Collectors.toList());

        if (!profilesToDelete.isEmpty()) {
            muProfileCRUDRepository.deleteAll(profilesToDelete);
        }
        //Добавляем профили МУ
        typesAdd.forEach(a -> {
            AreaTypes areaType = areaTypesCRUDRepository.findById(a).get();
            MuProfile muProfile = new MuProfile();
            muProfile.setAreaType(areaType);
            muProfile.setMuId(muId);
            muProfileCRUDRepository.save(muProfile);
        });
    }

    @Override
    public Long createPrimaryArea(long moId, long muId, String name, Integer number, String areaTypeCode,
                             Integer ageMin, Integer ageMax, Integer ageMinM, Integer ageMaxM, Integer ageMinW, Integer ageMaxW,
                             boolean autoAssignForAttachment, Boolean attachByMedicalReason) throws ContingentException {
        Validation validation = new Validation();

        areaChecker.checkAreaTypesExist(Collections.singletonList(areaTypeCode), validation, "areaTypeCode");

        MuProfile muProfile = muProfileRepository.getMuProfilesByMuId(muId).stream()
                .filter(p -> p.getAreaType() != null && Objects.equals(p.getAreaType().getCode(), areaTypeCode))
                .findFirst().orElse(null);

        if (muProfile == null) {
            validation.error(AreaErrorReason.MU_PROFILE_HAS_NO_AREA_TYPE, new ValidationParameter("areaTypeCode", areaTypeCode));
        }
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        Boolean mpguAvailable = muProfile.getAreaType().getAttributes() == null ? null : muProfile.getAreaType().getAttributes().getMpguAvailable();
        Boolean areaTypeAttachByMedicalReason = muProfile.getAreaType().getAttributes() == null ? null : muProfile.getAreaType().getAttributes().getAttachByMedicalReason();
        //Todo сделать проерку AREA_COUNT_LIMIT после разработки НСИ
        if (muProfile.getAreaType().getKindAreaType() != null &&
                //Todo уточнить код вида участка «Мягко - ассоциированный» и вынести в настройки
                Objects.equals(muProfile.getAreaType().getKindAreaType().getCode(), 1L)) {
            if (Strings.isNullOrEmpty(name) || number == null ||
                    (ageMin == null && ageMax == null && ageMinM == null && ageMaxM == null && ageMinW == null && ageMaxW == null)) {
                validation.error(AreaErrorReason.SOFT_RELATED_AREA_MUST_BE_FILLED);
            }
        }
        List<Area> areas = areaRepository.findAreas(null, muId, areaTypeCode, number, true);

        if (!areas.isEmpty()) {
            validation.error(AreaErrorReason.AREA_WITH_TYPE_AND_NUMBER_EXISTS_IN_MO, new ValidationParameter("areaTypeCode", areaTypeCode),
                    new ValidationParameter("number", number));
        }
        areaChecker.checkAreaTypeAgeSetups(muProfile.getAreaType(), ageMin, ageMax, ageMinM, ageMaxM, ageMinW, ageMaxW, validation);

        if (autoAssignForAttachment) {
            if (!Boolean.TRUE.equals(mpguAvailable)) {
                validation.error(AreaErrorReason.CANT_SET_AUTO_ASSIGN_FOR_ATTACHMENT, new ValidationParameter("areaTypeCode", areaTypeCode));
            }
            if (Boolean.TRUE.equals(attachByMedicalReason)) {
                validation.error(AreaErrorReason.AREA_FLAGS_INCORRECT);
            }
        }
        if (attachByMedicalReason != null && areaTypeAttachByMedicalReason != null &&
                !Objects.equals(attachByMedicalReason, areaTypeAttachByMedicalReason)) {
            validation.error(AreaErrorReason.ATTACH_BY_MEDICAL_REASON_INCORRECT,
                    new ValidationParameter("attachByMedicalReason", attachByMedicalReason),
                    new ValidationParameter("attachByMedicalReason", areaTypeAttachByMedicalReason));
        }
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        //Создание новго первичного участка
        Area area = new Area();
        area.setMoId(moId);
        area.setMuId(muId);
        area.setName(name);
        area.setNumber(number);
        area.setAreaType(muProfile.getAreaType());
        area.setActual(true);
        area.setAgeMax(ageMax);
        area.setAgeMin(ageMin);
        area.setAgeMMax(ageMaxM);
        area.setAgeMMin(ageMinM);
        area.setAgeWMax(ageMaxW);
        area.setAgeWMin(ageMinW);
        area.setAutoAssignForAttach(autoAssignForAttachment);
        area.setAttachByMedicalReason(attachByMedicalReason);
        areaCRUDRepository.save(area);

        return area.getId();
    }

    @Override
    public Long createDependantArea(long moId, long muId, String name, Integer number, String areaTypeCode, List<String> primaryAreaTypeCodes,
                             Integer ageMin, Integer ageMax, Integer ageMinM, Integer ageMaxM, Integer ageMinW, Integer ageMaxW,
                             boolean autoAssignForAttachment) throws ContingentException {
        Validation validation = new Validation();

        areaChecker.checkAreaTypesExist(Collections.singletonList(areaTypeCode), validation, "areaTypeCode");

        List<MuProfile> muProfiles = muProfileRepository.getMuProfilesByMuId(muId);
        Map<String, AreaTypes> primaryAreaTypes = muProfiles.stream()
                .filter(p -> p.getAreaType() != null)
                .collect(Collectors.toMap(p -> p.getAreaType().getCode(), MuProfile::getAreaType));

        primaryAreaTypeCodes.forEach(c -> {
            if (!primaryAreaTypes.keySet().contains(c)) {
                validation.error(AreaErrorReason.MU_PROFILE_HAS_NO_AREA_TYPE, new ValidationParameter("primaryAreaTypeCode", c));
            }
        });
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
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
        if (!areaRepository.findAreas(moId, null, areaTypeCode, null, null).isEmpty()) {
            validation.error(AreaErrorReason.AREA_WITH_TYPE_EXISTS_IN_MO, new ValidationParameter("areaTypeCode", areaTypeCode));
        }
        if (number != null &&
                !areaRepository.findAreas(null, muId, areaTypeCode, number, true).isEmpty()) {
            validation.error(AreaErrorReason.AREA_WITH_TYPE_AND_NUMBER_EXISTS_IN_MO, new ValidationParameter("areaTypeCode", areaTypeCode),
                    new ValidationParameter("number", number));
        }
        AreaTypes areaType = areaTypesCRUDRepository.findById(areaTypeCode).get();
        areaChecker.checkAreaTypeAgeSetups(areaType, ageMin, ageMax, ageMinM, ageMaxM, ageMinW, ageMaxW, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        //Создание новго зависимого участка
        Area area = new Area();
        area.setMoId(moId);
        area.setMuId(muId);
        area.setName(name);
        area.setNumber(number);
        area.setAreaType(areaType);
        area.setActual(true);
        area.setAgeMax(ageMax);
        area.setAgeMin(ageMin);
        area.setAgeMMax(ageMaxM);
        area.setAgeMMin(ageMinM);
        area.setAgeWMax(ageMaxW);
        area.setAgeWMin(ageMinW);
        area.setAutoAssignForAttach(autoAssignForAttachment);
        areaCRUDRepository.save(area);
        //Сохранение привязки к первичным типам участка
        primaryAreaTypeCodes.forEach(c -> {
            AreaToAreaType areaToAreaType = new AreaToAreaType();
            areaToAreaType.setArea(area);
            areaToAreaType.setAreaType(primaryAreaTypes.get(c));
            areaToAreaTypeCRUDRepository.save(areaToAreaType);
        });
        return area.getId();
    }
}
