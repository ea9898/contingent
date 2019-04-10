package moscow.ptnl.contingent.area.service;

import moscow.ptnl.contingent.area.entity.area.MuProfile;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypes;
import moscow.ptnl.contingent.area.entity.nsi.MUProfileTemplates;
import moscow.ptnl.contingent.area.error.AreaErrorReason;
import moscow.ptnl.contingent.area.error.Validation;
import moscow.ptnl.contingent.area.error.ValidationParameter;
import moscow.ptnl.contingent.area.repository.area.AreaRepository;
import moscow.ptnl.contingent.area.repository.area.MuProfileCRUDRepository;
import moscow.ptnl.contingent.area.repository.area.MuProfileRepository;
import moscow.ptnl.contingent.area.repository.nsi.AreaTypesCRUDRepository;
import moscow.ptnl.contingent.area.repository.nsi.MuProfileTemplatesRepository;
import moscow.ptnl.contingent.area.error.ContingentException;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;
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
            if (!areaRepository.findAreas(muId, a, true).isEmpty()) {
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
}
