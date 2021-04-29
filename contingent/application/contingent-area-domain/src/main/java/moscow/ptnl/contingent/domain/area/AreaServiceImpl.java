package moscow.ptnl.contingent.domain.area;


import moscow.ptnl.contingent.domain.AreaErrorReason;
import moscow.ptnl.contingent.domain.area.entity.AddressAllocationOrders;
import moscow.ptnl.contingent.domain.area.entity.Addresses;
import moscow.ptnl.contingent.domain.area.entity.Area;
import moscow.ptnl.contingent.domain.area.entity.AreaAddress;
import moscow.ptnl.contingent.domain.area.entity.AreaMedicalEmployees;
import moscow.ptnl.contingent.domain.area.entity.AreaPolicyTypes;
import moscow.ptnl.contingent.domain.area.entity.AreaToAreaType;
import moscow.ptnl.contingent.domain.area.entity.Area_;
import moscow.ptnl.contingent.domain.area.entity.MoAddress;
import moscow.ptnl.contingent.domain.area.heplers.AreaHelper;
import moscow.ptnl.contingent.domain.area.model.area.AddMedicalEmployee;
import moscow.ptnl.contingent.domain.area.model.area.AddressLevelType;
import moscow.ptnl.contingent.domain.area.model.area.AddressRegistry;
import moscow.ptnl.contingent.domain.area.model.area.AreaInfo;
import moscow.ptnl.contingent.domain.area.model.area.ChangeMedicalEmployee;
import moscow.ptnl.contingent.domain.area.model.area.MedicalEmployee;
import moscow.ptnl.contingent.domain.area.model.area.SearchAreaAddress;
import moscow.ptnl.contingent.domain.area.model.sysop.SysopMethodType;
import moscow.ptnl.contingent.domain.area.repository.AddressAllocationOrderRepository;
import moscow.ptnl.contingent.domain.area.repository.AddressesRepository;
import moscow.ptnl.contingent.domain.area.repository.AreaAddressRepository;
import moscow.ptnl.contingent.domain.area.repository.AreaMedicalEmployeeRepository;
import moscow.ptnl.contingent.domain.area.repository.AreaPolicyTypesRepository;
import moscow.ptnl.contingent.domain.area.repository.AreaRepository;
import moscow.ptnl.contingent.domain.area.repository.AreaToAreaTypeRepository;
import moscow.ptnl.contingent.domain.area.repository.MoAddressRepository;
import moscow.ptnl.contingent.domain.esu.event.AreaInfoEvent;
import moscow.ptnl.contingent.domain.esu.event.annotation.LogESU;
import moscow.ptnl.contingent.domain.history.EntityConverterHelper;
import moscow.ptnl.contingent.domain.util.Period;
import moscow.ptnl.contingent.error.ContingentException;
import moscow.ptnl.contingent.error.Validation;
import moscow.ptnl.contingent.error.ValidationParameter;
import moscow.ptnl.contingent.infrastructure.service.setting.SettingService;
import moscow.ptnl.contingent.nsi.domain.area.AreaType;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeKindEnum;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeMedicalPositions;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeSpecializations;
import moscow.ptnl.contingent.nsi.domain.area.PolicyType;
import moscow.ptnl.contingent.nsi.domain.area.PositionNom;
import moscow.ptnl.contingent.nsi.domain.area.Specialization;
import moscow.ptnl.contingent.nsi.domain.repository.AreaTypeMedicalPositionsRepository;
import moscow.ptnl.contingent.nsi.domain.repository.AreaTypeSpecializationsRepository;
import moscow.ptnl.contingent.nsi.domain.repository.AreaTypesRepository;
import moscow.ptnl.contingent.nsi.domain.repository.PolicyTypeRepository;
import moscow.ptnl.contingent.nsi.domain.repository.PositionNomRepository;
import moscow.ptnl.contingent.nsi.domain.repository.SpecializationRepository;
import moscow.ptnl.contingent.security.UserContextHolder;
import moscow.ptnl.util.Strings;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.StringUtils;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import static java.util.Comparator.naturalOrder;
import static java.util.Comparator.nullsFirst;

@Service
@Transactional
public class AreaServiceImpl implements AreaService {


    @Autowired
    private AreaAddressRepository areaAddressRepository;

    @Autowired
    private AreaRepository areaRepository;

    @Autowired
    private AreaHelper areaHelper;

    @Autowired
    private AreaMedicalEmployeeRepository areaMedicalEmployeeRepository;

    @Autowired
    private Algorithms algorithms;

    @Autowired
    @Lazy
    private HistoryServiceHelper historyHelper;

    @Autowired
    private AddressesRepository addressesRepository;

    @Autowired
    private SettingService settingService;

    @Autowired
    private AreaPolicyTypesRepository areaPolicyTypesRepository;

    @Autowired
    private PolicyTypeRepository policyTypeRepository;

    @Autowired
    private AreaTypesRepository areaTypesRepository;

    @Autowired
    private AreaToAreaTypeRepository areaToAreaTypeRepository;

    @Autowired
    private AreaTypeSpecializationsRepository areaTypeSpecializationsRepository;

    @Autowired
    private PositionNomRepository positionNomRepository;

    @Autowired
    @Lazy
    private EsuHelperService esuHelperService;

    @Autowired
    private SpecializationRepository specializationRepository;

    @Autowired
    private MoAddressRepository moAddressRepository;

    @Autowired
    private AreaTypeMedicalPositionsRepository areaTypeMedicalPositionsRepository;

    @Autowired
    private AddressAllocationOrderRepository addressAllocationOrderRepository;

    @Autowired
    @Lazy
    private MappingDomainService mappingDomainService;

    @Autowired
    private AreaServiceInternalAsync areaServiceInternalAsync;

    @Override @LogESU(type = AreaInfoEvent.class, useResult = true)
    public Long createPrimaryArea(long moId, Long muId, Integer number, Long areaTypeCode, List<Long> policyTypesIds,
                                  Integer ageMin, Integer ageMax, Integer ageMinM, Integer ageMaxM, Integer ageMinW, Integer ageMaxW,
                                  boolean autoAssignForAttachment, Boolean attachByMedicalReason, String description) throws ContingentException {
        return createPrimaryAreaInternal(moId, muId, number, areaTypeCode, policyTypesIds, ageMin, ageMax, ageMinM, ageMaxM, ageMinW, ageMaxW, autoAssignForAttachment, attachByMedicalReason, description).getId();
    }

    @Override
    public Area createPrimaryAreaInternal(long moId, Long muId, Integer number, Long areaTypeCode, List<Long> policyTypesIds,
                                          Integer ageMin, Integer ageMax, Integer ageMinM, Integer ageMaxM, Integer ageMinW, Integer ageMaxW,
                                          boolean autoAssignForAttachment, Boolean attachByMedicalReason, String description) throws ContingentException {
        Validation validation = new Validation();

        // 1
        List<AreaType> areaTypeList = areaHelper.checkAndGetAreaTypesExist(
                Collections.singletonList(areaTypeCode), validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        AreaType areaType = areaTypeList.get(0);
        // 2
        areaHelper.checkAreaTypeIsPrimary(areaType, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        // 3
        areaHelper.checkEmptyMuId(muId, areaType);
        // 4
        areaHelper.checkAreaTypeAvailable(moId, muId, areaType, validation);
        // 5
        areaHelper.checkAreaTypeCountLimits(moId, muId, areaType, validation);

        // 6
        if (areaType.getAreaTypeKind() != null &&
                Objects.equals(areaType.getAreaTypeKind().getCode(), AreaTypeKindEnum.MILDLY_ASSOCIATED.getCode()) &&
                (Strings.isNullOrEmpty(description) || number == null ||
                        ageMin == null && ageMax == null && ageMinM == null && ageMaxM == null && ageMinW == null && ageMaxW == null)) {
            validation.error(AreaErrorReason.SOFT_RELATED_AREA_MUST_BE_FILLED);
        }

        // 7
        areaHelper.checkAreaExistsInMU(muId, moId, areaType, number, null, validation);

        // 8
        areaHelper.checkPolicyTypesIsOMS(policyTypesIds, validation);

        // 9
        areaHelper.checkAreaTypeAgeSetups(areaType, ageMin, ageMax, ageMinM, ageMaxM, ageMinW, ageMaxW, validation);

        // 10
        areaHelper.checkAutoAssignForAttachment(areaType, autoAssignForAttachment, attachByMedicalReason, validation);

        // 11
        areaHelper.checkAttachByMedicalReason(areaType, attachByMedicalReason, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        // 12
        Area area = Area.builder()
                .moId(moId)
                .muId(muId)
                .areaType(areaType)
                .number(number)
                .autoAssignForAttach(autoAssignForAttachment)
                .archived(false)
                .description(description)
                .attachByMedicalReason(attachByMedicalReason)
                .ageMin(ageMin)
                .ageMax(ageMax)
                .ageMMin(ageMinM)
                .ageMMax(ageMaxM)
                .ageWMin(ageMinW)
                .ageWMax(ageMaxW)
                .createDate(LocalDateTime.now())
                .build();
        areaRepository.save(area);

        // 13
        List<PolicyType> policyTypes = policyTypeRepository.findByIds(policyTypesIds);
        List<AreaPolicyTypes> areaPolicyTypes = policyTypes.stream().map(policyType ->
                new AreaPolicyTypes(area, policyType)).collect(Collectors.toList());
        areaPolicyTypesRepository.saveAll(areaPolicyTypes);

        // 15
        historyHelper.sendHistory(null, area, Area.class);

        return area;
    }

    @Override
    public Long createDependentArea(long moId, Long muId, Integer number, Long areaTypeCode, List<Long> primaryAreaTypeCodesIds, List<Long> policyTypeCodesIds, Integer ageMin, Integer ageMax, Integer ageMinM, Integer ageMaxM, Integer ageMinW, Integer ageMaxW, String description) throws ContingentException {

        Validation validation = new Validation();

        //2
        List<AreaType> areaTypes = areaHelper.checkAndGetAreaTypesExist(Collections.singletonList(areaTypeCode), validation);
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        AreaType areaType = areaTypes.get(0);

        //3
        areaHelper.checkAreaTypeIsDependent(areaType, validation);
        //4
        primaryAreaTypeCodesIds = primaryAreaTypeCodesIds.stream().distinct().collect(Collectors.toList());

        primaryAreaTypeCodesIds.forEach(code -> {
            Optional<AreaType> primaryAreaTypeOptional = areaTypesRepository.findById(code);
            if (!primaryAreaTypeOptional.isPresent()) {
                validation.error(AreaErrorReason.AREA_TYPE_NOT_FOUND, new ValidationParameter("areaTypeCode", code));
                return;
            }
            AreaType primaryAreaType = primaryAreaTypeOptional.get();

            //4
            areaHelper.checkAreaTypeIsPrimary(primaryAreaType, validation);
            //5
            areaHelper.checkAreaTypeAvailable(moId, muId, primaryAreaType, validation);
            //6
            areaHelper.checkAreaTypeRelations(areaType, primaryAreaType, validation);
        });
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        //8
        if (!areaRepository.findAreas(moId, muId, areaTypeCode, null, true).isEmpty()) {
            validation.error(AreaErrorReason.AREA_WITH_TYPE_EXISTS_IN_MO, new ValidationParameter("areaTypeTitle", areaType.getTitle()));
        }
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        //9
        areaHelper.checkPolicyTypesIsOMS(policyTypeCodesIds, validation);

        //10
        areaHelper.checkAreaTypeAgeSetups(areaType, ageMin, ageMax, ageMinM, ageMaxM, ageMinW, ageMaxW, validation);
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        //11
        Area area = Area.builder()
                .moId(moId)
                .muId(muId)
                .areaType(areaType)
                .number(number)
                .archived(false)
                .description(description)
                .ageMin(ageMin)
                .ageMax(ageMax)
                .ageMMin(ageMinM)
                .ageMMax(ageMaxM)
                .ageWMin(ageMinW)
                .ageWMax(ageMaxW)
                .createDate(LocalDateTime.now())
                .build();
        areaRepository.save(area);

        //12.
        for (Long areaTypeCodeId : primaryAreaTypeCodesIds) {
            AreaToAreaType areaToAreaType = new AreaToAreaType();
            areaToAreaType.setArea(area);
            areaToAreaType.setAreaType(areaTypesRepository.findById(areaTypeCodeId).get());
            areaToAreaTypeRepository.save(areaToAreaType);
        }
        //13
        List<PolicyType> policyTypeList = policyTypeRepository.findByIds(policyTypeCodesIds);

        if (policyTypeCodesIds != null && !policyTypeCodesIds.isEmpty()) {
            AreaPolicyTypes areaPolicyTypes = new AreaPolicyTypes();
            areaPolicyTypes.setArea(area);
            areaPolicyTypes.setPolicyType(policyTypeList.get(0));
            areaPolicyTypesRepository.save(areaPolicyTypes);
        }

        //14
        List<Area> primAreas = areaRepository.findAreas(moId, muId, primaryAreaTypeCodesIds, null, true);

        //15
        if (primAreas != null && !primAreas.isEmpty()) {
            esuHelperService.sendAttachOnAreaChangeEvent(primAreas.stream().map(Area::getId).collect(Collectors.toList()),
                    null, area);
        }

        //16
        historyHelper.sendHistory(null, area, Area.class);

        //17
        return area.getId();
    }

    @Override @LogESU(type = AreaInfoEvent.class, parameters = {"areaId"})
    public void updatePrimaryArea(long areaId, Integer number, List<Long> policyTypesAddIds, List<Long> policyTypesDelIds, Integer ageMin, Integer ageMax, Integer ageMinM, Integer ageMaxM, Integer ageMinW, Integer ageMaxW, Boolean autoAssignForAttachment, Boolean attachByMedicalReason, String description) throws ContingentException {
        Validation validation = new Validation();

        // 1, 2
        Area area = areaHelper.checkAndGetArea(areaId, validation);
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        Area oldArea = historyHelper.clone(area);

        //3 Система проверяет, что передан хотя бы один параметр для изменения, иначе возвращает ошибку
        areaHelper.checkAreaParametersForUpdate(number, policyTypesAddIds, policyTypesDelIds,
                ageMin, ageMax, ageMinM, ageMaxM, ageMinW, ageMaxW,
                autoAssignForAttachment, attachByMedicalReason, description, validation);

        //4 Система проверяет, что переданные параметры изменены, иначе возвращает ошибку
        List<PolicyType> policyTypesAdd = policyTypeRepository.findByIds(policyTypesAddIds);
        List<PolicyType> policyTypesDel = policyTypeRepository.findByIds(policyTypesDelIds);
        areaHelper.checkAreaParametersForUpdateChanged(area, number,
                policyTypesAdd, policyTypesDel, ageMin, ageMax, ageMinM, ageMaxM,
                ageMinW, ageMaxW, autoAssignForAttachment, attachByMedicalReason,
                description, validation);

        // 3
        if (number != null) {
            areaHelper.checkAreaExistsInMU(area.getMuId(), area.getMoId(), area.getAreaType(), number, area.getId(), validation);
        }

        // 4
        areaHelper.checkPolicyTypesIsOMS(policyTypesAddIds, validation);

        // 5
        areaHelper.checkPolicyTypesDel(area, policyTypesDel, validation);

        // 6
        areaHelper.checkAutoAssignForAttachment(area.getAreaType(), autoAssignForAttachment, attachByMedicalReason, validation);

        // 7
        areaHelper.checkAttachByMedicalReason(area.getAreaType(), attachByMedicalReason, validation);

        // 8
        areaHelper.checkAreaTypeAgeSetups(area.getAreaType(), ageMin, ageMax, ageMinM, ageMaxM, ageMinW, ageMaxW, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        // 9
        area.setNumber(number == null ? area.getNumber() : number);
        area.setAgeMax(ageMax == null ? area.getAgeMax() : ageMax);
        area.setAgeMin(ageMin == null ? area.getAgeMin() : ageMin);
        area.setAgeMMax(ageMaxM == null ? area.getAgeMMax() : ageMaxM);
        area.setAgeMMin(ageMinM == null ? area.getAgeMMin() : ageMinM);
        area.setAgeWMax(ageMaxW == null ? area.getAgeWMax() : ageMaxW);
        area.setAgeWMin(ageMinW == null ? area.getAgeWMin() : ageMinW);
        area.setAutoAssignForAttach(autoAssignForAttachment != null ? autoAssignForAttachment : area.getAutoAssignForAttach());
        area.setAttachByMedicalReason(attachByMedicalReason == null ? area.getAttachByMedicalReason() : attachByMedicalReason);
        area.setDescription(description == null ? area.getDescription() : description);
        area.setUpdateDate(LocalDateTime.now());
        areaHelper.resetAutoAssignForAttachment(area);
        areaRepository.save(area);

        // 10
        areaHelper.saveAndDeleteAreaPolicyTypes(area, policyTypesAdd, policyTypesDel);

        // Логирование изменений
        historyHelper.sendHistory(oldArea, area, Area.class);
    }

    @Override
    public void updateDependentArea(long areaId, Long muId, Integer number, String description, List<Long> primaryAreaTypeCodesAddIds, List<Long> primaryAreaTypeCodesDelIds, List<Long> policyTypesAddIds, List<Long> policyTypesDelIds, Integer ageMin, Integer ageMax, Integer ageMinM, Integer ageMaxM, Integer ageMinW, Integer ageMaxW) throws ContingentException {
        Validation validation = new Validation();
        //1, 2.
        Area area = areaHelper.checkAndGetArea(areaId, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        //3, 4.
        areaHelper.checkParametersChanged(area, muId, number, description, primaryAreaTypeCodesAddIds,
                primaryAreaTypeCodesDelIds, policyTypesAddIds, policyTypesDelIds, ageMin, ageMax, ageMinM,
                ageMaxM, ageMinW, ageMaxW, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        Area oldArea = historyHelper.clone(area);

        // 5.
        if (muId != null && (area.getAreaType().getAreaTypeKind() == null ||
                !AreaTypeKindEnum.TREATMENT_ROOM_ASSOCIATED.equalsCode(area.getAreaType().getAreaTypeKind().getCode()))) {
            validation.error(AreaErrorReason.AREA_NOT_RELATED_TO_SPECIAL_OFFICE);
        }
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        // 6.
        primaryAreaTypeCodesAddIds = primaryAreaTypeCodesAddIds.stream().distinct().collect(Collectors.toList());
        primaryAreaTypeCodesDelIds = primaryAreaTypeCodesDelIds.stream().distinct().collect(Collectors.toList());

        primaryAreaTypeCodesAddIds.forEach(pat -> {
                    Optional<AreaType> primaryAreaTypeOptional = areaTypesRepository.findById(pat);

                    if (!primaryAreaTypeOptional.isPresent()) {
                        validation.error(AreaErrorReason.AREA_TYPE_NOT_FOUND, new ValidationParameter("areaTypeCode", pat));
                        return;
                    }

                    AreaType primaryAreaType = primaryAreaTypeOptional.get();
                    // 7.1
                    areaHelper.checkAreaDependsOnPrimaryAreaType(area, primaryAreaType, validation);
                    // 7.2
                    areaHelper.checkAreaTypeIsPrimary(primaryAreaType, validation);
                    // 7.3
                    areaHelper.checkAreaTypeAvailable(area.getMoId(), muId, primaryAreaType, validation);
                    // 7.4
                    areaHelper.checkAreaTypeRelations(area.getAreaType(), primaryAreaType, validation);
                }
        );

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        // 8.
        List<AreaToAreaType> areaTypeToDeleteBd = null;
        if (!primaryAreaTypeCodesDelIds.isEmpty()) {
            areaTypeToDeleteBd = areaToAreaTypeRepository.findAreaTypesByAreaAndTypeCode(area, primaryAreaTypeCodesDelIds);
            for (Long areaTypeToDelete : primaryAreaTypeCodesDelIds) {
                if (areaTypeToDeleteBd.stream().noneMatch(areaType -> areaType.getAreaType().getCode().equals(areaTypeToDelete))) {
                    validation.error(AreaErrorReason.AREA_NOT_DEPEND_ON_AREA_TYPE,
                            new ValidationParameter("areaId", areaId),
                            new ValidationParameter("areTypeTitle", areaTypesRepository.findById(areaTypeToDelete).get().getTitle()));
                }
            }
            if (!validation.isSuccess()) {
                throw new ContingentException(validation);
            }
        }

        // 9.
        areaHelper.checkPolicyTypesIsOMS(policyTypesAddIds, validation);
        List<PolicyType> policyTypesAdd = policyTypeRepository.findByIds(policyTypesAddIds);

        // 10.
        List<PolicyType> policyTypesDel = policyTypeRepository.findByIds(policyTypesDelIds);
        policyTypesDel.forEach(ptd -> {
            List<AreaPolicyTypes> areaPolicyTypes = areaPolicyTypesRepository.findAll(area, ptd);
            if (areaPolicyTypes.isEmpty()) {
                validation.error(AreaErrorReason.POLICY_TYPE_NOT_SET_FOR_AREA,
                        new ValidationParameter("policyCode", ptd.getCode()),
                        new ValidationParameter("areaId", area.getId()));
            }
        });
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        // 11.
        areaHelper.checkAreaTypeAgeSetups(area.getAreaType(), ageMin, ageMax, ageMinM, ageMaxM, ageMinW, ageMaxW, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        // 12.
        Long muIdFinal = muId == null ? area.getMuId() : muId;
        area.setMuId(muIdFinal);
        area.setNumber(number == null ? area.getNumber() : number);
        area.setAgeMax(ageMax == null ? area.getAgeMax() : ageMax);
        area.setAgeMin(ageMin == null ? area.getAgeMin() : ageMin);
        area.setAgeMMax(ageMaxM == null ? area.getAgeMMax() : ageMaxM);
        area.setAgeMMin(ageMinM == null ? area.getAgeMMin() : ageMinM);
        area.setAgeWMax(ageMaxW == null ? area.getAgeWMax() : ageMaxW);
        area.setAgeWMin(ageMinW == null ? area.getAgeWMin() : ageMinW);
        area.setDescription(description == null ? area.getDescription() : description);
        area.setUpdateDate(LocalDateTime.now());
        areaRepository.save(area);

        // 13.
        // 13.1.
        primaryAreaTypeCodesAddIds.forEach(pata -> {
            AreaType areaType = areaTypesRepository.findById(pata).get();
            AreaToAreaType areaToAreaType = new AreaToAreaType();
            areaToAreaType.setArea(area);
            areaToAreaType.setAreaType(areaType);
            areaToAreaTypeRepository.save(areaToAreaType);
        });
        // 13.2.
        if (!primaryAreaTypeCodesDelIds.isEmpty()) {
            areaToAreaTypeRepository.deleteAll(areaTypeToDeleteBd);
        }

        // 14.
        areaHelper.saveAndDeleteAreaPolicyTypes(area, policyTypesAdd, policyTypesDel);

        // Вид участка не именной
        if (area.getAreaType() != null && area.getAreaType().getAreaTypeKind() != null &&
                !area.getAreaType().getAreaTypeKind().getCode().equals(AreaTypeKindEnum.PERSONAL.getCode())) {
            // 15.
            if (!primaryAreaTypeCodesAddIds.isEmpty()) {
                // 15.1.
                List<Area> primAreasAdd = areaRepository.findAreas(area.getMoId(), area.getMuId(), primaryAreaTypeCodesAddIds, null, true);

                // 15.2.
                if (primAreasAdd != null && !primAreasAdd.isEmpty()) {
                    esuHelperService.sendAttachOnAreaChangeEvent(
                            primAreasAdd.stream().map(Area::getId).collect(Collectors.toList()),
                            null,
                            area);
                }
            }

            // 16.
            if (!primaryAreaTypeCodesDelIds.isEmpty()) {
                // 16.1.
                List<Area> primAreasDel = areaRepository.findAreas(area.getMoId(), area.getMuId(), primaryAreaTypeCodesDelIds, null, true);
                // 16.2.
                if (primAreasDel != null && !primAreasDel.isEmpty()) {
                    esuHelperService.sendAttachOnAreaChangeEvent(
                            null,
                            primAreasDel.stream().map(Area::getId).collect(Collectors.toList()),
                            area);
                }
            }
        }

        // Логирование изменений
        historyHelper.sendHistory(oldArea, area, Area.class);

        // 17.

    }

    @Override @LogESU(type = AreaInfoEvent.class, parameters = {"areaId"})
    public List<Long> setMedicalEmployeeOnArea(long areaId, List<AddMedicalEmployee> addEmployeesInput,
                                               List<ChangeMedicalEmployee> changeEmployeesInput) throws ContingentException {
        return setMedicalEmployeeOnAreaInternal(areaId, addEmployeesInput, changeEmployeesInput);
    }

    @Override
    public List<Long> setMedicalEmployeeOnAreaInternal(long areaId, List<AddMedicalEmployee> addEmployeesInput,
                                                       List<ChangeMedicalEmployee> changeEmployeesInput) throws ContingentException {

        Validation validation = new Validation();

        // 1. 2.
        Area area = areaHelper.checkAndGetArea(areaId, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        //3
        List<Long> changeIds = changeEmployeesInput.stream().map(ChangeMedicalEmployee::getAssignmentId)
                .collect(Collectors.toList());
        List<AreaMedicalEmployees> changeEmployeesDb = new ArrayList<>();
        areaMedicalEmployeeRepository.findAllById(changeIds).forEach(changeEmployeesDb::add);

        if (!AreaTypeKindEnum.MILDLY_ASSOCIATED.equalsCode(area.getAreaType().getAreaTypeKind().getCode())
                && !AreaTypeKindEnum.TREATMENT_ROOM_ASSOCIATED.equalsCode(area.getAreaType().getAreaTypeKind().getCode()))
        {

            addEmployeesInput.stream().filter(empl -> !empl.isReplacement())
                    .forEach(empl -> validation.error(AreaErrorReason.AREA_NOT_RELATED_TO_MILDLY_ASSOCIATED));

            changeEmployeesDb.stream().filter(empl -> !empl.getReplacement())
                    .forEach(empl -> validation.error(AreaErrorReason.AREA_NOT_RELATED_TO_MILDLY_ASSOCIATED));
        }
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        //4
        List<AreaMedicalEmployees> areaEmployeesDb = areaMedicalEmployeeRepository.getEmployeesByAreaId(areaId);

        for (ChangeMedicalEmployee inputEmpl : changeEmployeesInput) {

            Optional<AreaMedicalEmployees> employee = areaEmployeesDb.stream().filter(
                    empl -> empl.getId().equals(inputEmpl.getAssignmentId())).findFirst();
            AreaMedicalEmployees emplDb = null;
            if (!employee.isPresent()) {
                validation.error(AreaErrorReason.EMPLOYEE_NOT_RELATED_TO_AREA,
                        new ValidationParameter("assignmentId", inputEmpl.getAssignmentId()));
            } else {
                emplDb = employee.get();
                //4.1
                if (inputEmpl.getEndDate() != null && inputEmpl.getEndDate().isBefore(LocalDate.now())
                        || employee.get().getArea().getId() != areaId) {
                    validation.error(AreaErrorReason.EMPLOYEE_NOT_RELATED_TO_AREA,
                            new ValidationParameter("assignmentId", inputEmpl.getAssignmentId()));
                }
            }

            if(inputEmpl.isIsError() == null || !inputEmpl.isIsError()) {
                //5.2.2.1
                if (inputEmpl.getEndDate() == null && inputEmpl.getStartDate() == null) {
                    validation.error(AreaErrorReason.NOTHING_TO_CHANGE);
                }

                //5.2.2.2
                LocalDate startDate = inputEmpl.getStartDate() != null ? inputEmpl.getStartDate()
                        : emplDb != null ? emplDb.getStartDate() : null;
                LocalDate endDate = inputEmpl.getEndDate() != null ? inputEmpl.getEndDate()
                        : emplDb != null ? emplDb.getStartDate() : null;
                if (startDate != null && endDate != null && startDate.isAfter(endDate)) {
                    validation.error(AreaErrorReason.START_DATE_IS_AFTER_END_DATE,
                            new ValidationParameter("endDate", endDate),
                            new ValidationParameter("startDate", startDate));
                }
            }
        }
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        //5
        List<AreaTypeSpecializations> areaTypeSpecializations = areaTypeSpecializationsRepository.
                findByAreaTypeCode(area.getAreaType());
        for (AddMedicalEmployee empl : addEmployeesInput) {
            //5.1.
            if (empl.getStartDate().isBefore(LocalDate.now())) {
                validation.error(AreaErrorReason.START_DATE_IN_PAST,
                        new ValidationParameter("startDate", empl.getStartDate()));
            }

            // 5.2.
            if (empl.getEndDate() != null && empl.getStartDate().isAfter(empl.getEndDate())){
                validation.error(AreaErrorReason.START_DATE_IS_AFTER_END_DATE,
                        new ValidationParameter("endDate", empl.getEndDate()),
                        new ValidationParameter("startDate", empl.getStartDate()));
            }

            //5.3.

            // 5.3.1.
            // 5.3.2.
            Optional<PositionNom> positionNomOptional = positionNomRepository.getByPositionCode(empl.getPositionCode());

            if (!positionNomOptional.isPresent()) {
                // TODO ???
                continue;
            }

            //5.4.
            PositionNom positionNom = positionNomOptional.get();
            Specialization specialization = positionNom.getSpecialization();

            // 5.5.
            if (specialization != null && areaTypeSpecializations.stream().noneMatch(ats -> ats.getSpecializationCode().equals(specialization.getCode()))) {
                validation.error(AreaErrorReason.SPECIALIZATION_NOT_RELATED_TO_AREA,
                        new ValidationParameter("SpecializationTitle", specialization.getTitle()),
                        new ValidationParameter("jobInfoId", empl.getMedicalEmployeeJobInfoId()),
                        new ValidationParameter("AreaSpecializationTitles", areaTypeSpecializations.stream()
                                .map(AreaTypeSpecializations::getSpecializationCode)
                                .distinct()
                                .map(specializationRepository::getByCode)
                                .map(Specialization::getTitle)
                                .collect(Collectors.joining(", "))));
            }

            // 5.6.
            List<AreaTypeMedicalPositions> positions = areaTypeMedicalPositionsRepository.getPositionsByAreaType(area.getAreaType().getCode());

            if (positions != null && !positions.isEmpty() && positions.stream().noneMatch(pos -> pos.getPositionCode().getCode().equals(empl.getPositionCode()))) {
                validation.error(AreaErrorReason.POSITION_NOT_SET_FOR_AREA_TYPE,
                        new ValidationParameter("positionTitle", positionNom.getTitle()),
                        new ValidationParameter("jobInfoId", empl.getMedicalEmployeeJobInfoId()),
                        new ValidationParameter("areaTypeTitle", area.getAreaType().getTitle()));
            }
        }
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        //6.1
        List<AreaMedicalEmployees> allEmployees = new ArrayList<>(areaEmployeesDb);
        Map<AreaMedicalEmployees, AreaMedicalEmployees> changesAme =  areaHelper.applyChanges(allEmployees, changeEmployeesInput);
        areaHelper.addNew(allEmployees, addEmployeesInput, area);
        areaHelper.checkDatesNotInterceptWithSamePosition(
                allEmployees.stream().filter(me -> me.getError() == null || !me.getError()).collect(Collectors.toList()), validation); // отфитровываем ошибочно назначенные работников из проверки

        //6.2
        List<AreaMedicalEmployees> mainEmployees =
                areaEmployeesDb.stream().filter(empl -> !empl.getReplacement()).collect(Collectors.toList());
        areaHelper.applyChanges(mainEmployees, changeEmployeesInput);
        areaHelper.addNew(mainEmployees, addEmployeesInput.stream()
                .filter(empl -> !empl.isReplacement()).collect(Collectors.toList()), area);
        if (area.getAreaType() != null && area.getAreaType().getAreaTypeKind() != null &&
                area.getAreaType().getAreaTypeKind().getCode() == AreaTypeKindEnum.MILDLY_ASSOCIATED.getCode()) {
            areaHelper.checkMainEmployeesOverlappingDates(
                    mainEmployees.stream().filter(me -> me.getError() == null || !me.getError()).collect(Collectors.toList()), validation);  // отфитровываем ошибочно назначенные работников из проверки
        }

        //6.3
        if (area.getAreaType() != null &&
                area.getAreaType().getAreaTypeKind() != null &&
                (area.getAreaType().getAreaTypeKind().getCode().equals(AreaTypeKindEnum.MILDLY_ASSOCIATED.getCode()) ||
                        area.getAreaType().getAreaTypeKind().getCode().equals(AreaTypeKindEnum.TREATMENT_ROOM_ASSOCIATED.getCode()))) {
            //6.3.1.
            List<Period> periodsWithoutMainEmpl = areaHelper.getPeriodsWithoutMainEmployee(mainEmployees);
            if (periodsWithoutMainEmpl.size() > 0) {
                //6.3.2.
                List<AreaMedicalEmployees> replacementEmployees = areaEmployeesDb.stream()
                        .filter(AreaMedicalEmployees::getReplacement).collect(Collectors.toList());
                areaHelper.applyChanges(replacementEmployees, changeEmployeesInput);
                areaHelper.addNew(replacementEmployees, addEmployeesInput.stream()
                        .filter(AddMedicalEmployee::isReplacement).collect(Collectors.toList()), area);
                replacementEmployees.sort(Comparator.comparing(AreaMedicalEmployees::getStartDate, nullsFirst(naturalOrder())));
//                areaHelper.checkReplacementWithoutMain(periodsWithoutMainEmpl, replacementEmployees, validation); https://jira.emias.mos.ru/browse/CONTINGENT2-643
            }
        }

        //7
        List<Long> result = new ArrayList<>();
        areaHelper.applyChanges(changeEmployeesDb, changeEmployeesInput);
        areaHelper.addNew(changeEmployeesDb, addEmployeesInput, area);
        areaMedicalEmployeeRepository.saveAll(changeEmployeesDb).forEach(saved -> {
            if (!areaEmployeesDb.contains(saved)) {
                // Логирование новых объектов
                historyHelper.sendHistory(null, saved, AreaMedicalEmployees.class);
                result.add(saved.getId());
            }
        });

        // Логирование изменений
        for (Map.Entry<AreaMedicalEmployees, AreaMedicalEmployees> entry: changesAme.entrySet()) {
            historyHelper.sendHistory(entry.getKey(), entry.getValue(), AreaMedicalEmployees.class);
        }

        // 8
        //if (areaHelper.isAreaPrimary(area)) {
        //    esuHelperService.sendAreaInfoEvent(area, "setMedicalEmployeeOnArea");
        //}

        return result;
    }

    @Override
    public AreaInfo getAreaById(Long areaId) throws ContingentException {
        // 1.
        Optional<Area> areaOptional = areaRepository.findById(areaId);
        if (!areaOptional.isPresent()) {
            throw new ContingentException(new Validation().error(AreaErrorReason.AREA_NOT_FOUND, new ValidationParameter("areaId", areaId)));
        }
        Area area = areaOptional.get();

        // 2.
        List<AreaMedicalEmployees> mainMedicalEmployees = areaMedicalEmployeeRepository.
                getEmployeesMainActualByAreaId(area.getId());

        // 3.
        List<AreaMedicalEmployees> replacementMedicalEmployees = areaMedicalEmployeeRepository.
                getEmployeesReplacementActualByAreaId(area.getId());

        return new AreaInfo(area, mainMedicalEmployees, replacementMedicalEmployees);
    }

    @Override
    public AreaInfo getAreaByIdV2(Long areaId) throws ContingentException {
        AreaInfo areaInfo = getAreaById(areaId);

        areaInfo.setAreaTypeProfile(areaInfo.getArea().getAreaTypeProfile());
        areaInfo.setAreaMuServices(new ArrayList<>(areaInfo.getArea().getActualAreaMuServices()));

        return areaInfo;
    }

    @Override  @LogESU(type = AreaInfoEvent.class, parameters = {"areaId"})
    public List<Long> addAreaAddress(Long areaId, List<AddressRegistry> addressesRegistry, boolean limitAddress) throws ContingentException {
        return addAreaAddressInternal(areaId, addressesRegistry, limitAddress);
    }

    @Override
    public List<Long> addAreaAddressInternal(Long areaId, List<AddressRegistry> addressesRegistry, boolean limitAddress) throws ContingentException {
        Validation validation = new Validation();

        // 2 и 3
        Area area = areaHelper.checkAndGetArea(areaId, validation);
        if (!validation.isSuccess()) { throw new ContingentException(validation); }

        // 4
        if (limitAddress) {
            areaHelper.checkTooManyAddresses(addressesRegistry, settingService.getPar1());
        }

        // 5
        Set<Long> addrSet = new HashSet<>(addressesRegistry.size());
        addressesRegistry.removeIf(addr -> !addrSet.add(addr.getGlobalIdNsi()));

        // 6
        algorithms.checkAddressFLK(addressesRegistry, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        // 7
        Map<Long, MoAddress> findMoAddress = new HashMap<>();
        addressesRegistry.forEach(ar -> {
            MoAddress moAddressIntersect = algorithms.searchServiceDistrictMOByAddress(area.getAreaType(),
                    ar, validation);
            if (moAddressIntersect == null || !moAddressIntersect.getMoId().equals(area.getMoId())) {
                validation.error(AreaErrorReason.ADDRESS_NOT_SERVICED_MO_NSI, new ValidationParameter("addressString",  ar.getAddressString()),
                        new ValidationParameter("moId", area.getMoId()));
            } else if (moAddressIntersect.getMoId().equals(area.getMoId())) {
                findMoAddress.put(ar.getGlobalIdNsi(), moAddressIntersect);
            }
        });

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        // 8
        List<AreaAddress> intersectingAddresses = algorithms.searchAreaByAddress(area.getMoId(), area.getAreaType(), addressesRegistry, validation);
        intersectingAddresses.forEach(a -> {
            if (!a.getArea().equals(area)) {
                validation.error(AreaErrorReason.ADDRESS_ALREADY_SERVICED_ANOTHER_AREA,
                        new ValidationParameter("globalIdNsi", a.getAddress().getGlobalId()),
                        new ValidationParameter("areaId", a.getArea().getId()),
                        new ValidationParameter("areaTypeCode", area.getAreaType().getCode()));
            } else {
                String addressString = addressesRegistry.stream()
                        .filter(r -> Objects.equals(r.getGlobalIdNsi(), a.getAddress().getGlobalId()))
                        .map(AddressRegistry::getAddressString)
                        .findFirst()
                        .orElse(a.getAddress().getAddress());
                validation.error(AreaErrorReason.ADDRESS_ALREADY_SERVICED_NSI, new ValidationParameter("addressString", addressString));
            }
        });
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        // 9
        List<Addresses> addresses = areaHelper.getMoAreaAddresses(addressesRegistry.stream().map(ar -> mappingDomainService.dtoToEntityTransform(ar)).collect(Collectors.toList()));

        // 10
        List<AreaAddress> areaAddresses = addresses.stream().map(addr -> {
            AreaAddress areaAddress = new AreaAddress();
            areaAddress.setArea(area);
            areaAddress.setMoAddress(findMoAddress.get(addr.getGlobalId()));
            areaAddress.setAddress(addr);
            areaAddress.setCreateDate(LocalDateTime.now());
            areaAddress.setUpdateDate(LocalDateTime.now());
            areaAddress.setStartDate(LocalDate.now());
            return areaAddress;
        }).collect(Collectors.toList());
        areaAddressRepository.saveAll(areaAddresses);

        // 11 аннотация @LogESU

        // 12
        for (AreaAddress areaAddress: areaAddresses) {
            historyHelper.sendHistory(null, areaAddress, AreaAddress.class);
        }

        return areaAddresses.stream().map(AreaAddress::getId).collect(Collectors.toList());
    }

    @Override @LogESU(type = AreaInfoEvent.class, parameters = {"areaId"})
    public void delAreaAddress(long areaId, List<Long> areaAddressIds) throws ContingentException {
        Validation validation = new Validation();

        // 1. и 2.
        Area area = areaHelper.checkAndGetArea(areaId, validation);
        if (!validation.isSuccess()) { throw new ContingentException(validation); }

        // 3.
        areaHelper.tooManyAreaAddresses(areaAddressIds, settingService.getPar2(), validation);
        if (!validation.isSuccess()) { throw new ContingentException(validation); }

        // 4.
        List<AreaAddress> areaAddresses = areaAddressRepository.findAreaAddressesActual(areaAddressIds);
        areaAddressIds.forEach(aai -> {
            List<Long> areaAddressesIdsDiff = areaAddresses.stream().map(AreaAddress::getId).collect(Collectors.toList());
            if (areaAddresses.stream().map(AreaAddress::getId).noneMatch(aa -> aa.equals(aai))) {
                validation.error(AreaErrorReason.MO_ADDRESS_NOT_EXISTS, new ValidationParameter("addressId", aai));
            }
        });
        if (!validation.isSuccess()) { throw new ContingentException(validation); }

        // 5.
        LocalDate localDate = LocalDate.now();
        for (AreaAddress areaAddress: areaAddresses) {
            if (areaAddress.getStartDate() == null || !areaAddress.getStartDate().equals(localDate)) {
                AreaAddress areaAddressOld = areaAddress.clone();
                // 5.1.
                areaAddress.setEndDate(localDate.minusDays(1L));
                areaAddress.setUpdateDate(LocalDateTime.now());
                areaAddressRepository.save(areaAddress);
                historyHelper.sendHistory(areaAddressOld, areaAddress, AreaAddress.class);
            } else {
                historyHelper.sendHistory(areaAddress, null, AreaAddress.class);

                // 5.2.
                areaAddressRepository.delete(areaAddress);
            }
        }

        // 6. @LogEsu

        // 7.

    }

    @Override
    public Page<AreaAddress> getAreaAddress(Long moId, List<Long> areaIds, PageRequest paging) throws ContingentException {
        Validation validation = new Validation();

        // 2.
        areaHelper.checkMaxPage(paging, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        List<Area> areas = areaRepository.findAllById(areaIds);
        // 3.
        if (areaIds.size() != areas.size()) {
            List<Long> foundAreasIds = areas.stream().map(Area::getId).collect(Collectors.toList());
            areaIds.stream().filter(aIn -> !foundAreasIds.contains(aIn)).forEach(aIn ->
                    validation.error(AreaErrorReason.AREA_NOT_FOUND, new ValidationParameter("areaId", aIn)));

            if (!validation.isSuccess()) { throw new ContingentException(validation); }
        }

        // 4.
        List<Long> areaIdsNotInMo = areas.stream().filter(area -> !area.getMoId().equals(moId))
                .map(Area::getId).collect(Collectors.toList());
        if (!areaIdsNotInMo.isEmpty())  {
            validation.error(AreaErrorReason.AREAS_NOT_IN_MO,
                    new ValidationParameter("areaIds", areaIdsNotInMo.stream().map(Object::toString)
                            .collect(Collectors.joining(","))),
                    new ValidationParameter("moId", moId));
        }
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        // 5.
        Page<AreaAddress> areaAddresses = areaAddressRepository.findAreaAddressesByAreaId(moId, areaIds, paging);

        return new PageImpl<>(new ArrayList<>(areaAddresses.getContent()),
                areaAddresses.getPageable(), areaAddresses.getTotalElements());
    }

    @Override @LogESU(type = AreaInfoEvent.class, parameters = {"areaId"})
    public void archiveArea(long areaId) throws ContingentException {
        Validation validation = new Validation();

        // 1. 2.
        Area area = areaHelper.checkAndGetArea(areaId, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        Area oldArea = historyHelper.clone(area);

        // 3.
        if (area != null && Boolean.TRUE.equals(area.getAutoAssignForAttach())) {
            validation.error(AreaErrorReason.AREA_IS_AUTO_ATTACH, new ValidationParameter("areaId", areaId));
        }
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        // 4. Система исключает адреса обслуживаня из участка, если указаны
        areaHelper.delAreaAddresses(new ArrayList<>(area.getActualAreaAddresses()));

        // 5. Система исключает МР из участка, если указаны
        if (area.getActualMedicalEmployees() != null && !area.getActualMedicalEmployees().isEmpty()) {
            areaHelper.delAreaMedicalEmployees(new ArrayList<>(area.getActualMedicalEmployees()));
        }

        // 6. Система для данного участка меняет статус на «Архивный»
        area.setArchived(true);
        area.setUpdateDate(LocalDateTime.now());
        areaRepository.save(area);

        // 7.
        //if (areaHelper.isAreaPrimary(area)) {
        //    esuHelperService.sendAreaInfoEvent(area, "archiveArea");
        //}

        // 9
        historyHelper.sendHistory(oldArea, area, Area.class);

    }

    @Override @LogESU(type = AreaInfoEvent.class, parameters = {"areaId"})
    public void restoreArea(Long areaId) throws ContingentException {
        Validation validation = new Validation();

        // 1. 2.
        Area area = areaHelper.checkAndGetArchivedArea(areaId, validation);

        if (area != null) {
            // 3.
            areaHelper.checkAreaTypeIsNotPersonal(area.getAreaType(), validation);

            // 4.
            areaHelper.checkAreaExistsInMU(area.getMuId(), area.getMoId(), area.getAreaType(), area.getNumber(), area.getId(), validation);
        }
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        Area oldArea = historyHelper.clone(area);

        // 5.
        area.setAutoAssignForAttach(false);

        // 6.
        area.setArchived(false);
        area.setUpdateDate(LocalDateTime.now());
        areaRepository.save(area);

        // 8.
        if (areaHelper.isAreaDependent(area)) {
            List<Area> areas = areaRepository.findPrimaryAreasByAreaEqAreaType(area);

            if (!areas.isEmpty()) {
                esuHelperService.sendAttachOnAreaChangeEvent(
                        areas.stream().map(Area::getId).collect(Collectors.toList()),
                        null,
                        area
                );
            }
        }

        // 9. @LogESU

        // 10.
        historyHelper.sendHistory(oldArea, area, Area.class);

        // 11
        return;
    }

    @Override
    public List<Long> addMoAddress(long moId, long areaTypeCode, long orderId, List<AddressRegistry> addressesRegistry, boolean limitAddress) throws ContingentException {
        Validation validation = new Validation();
        // 1.
        if (limitAddress) {
            areaHelper.checkTooManyAddresses(addressesRegistry, settingService.getPar1());
        }

        // 2.
        List<AreaType> areaTypes = areaHelper.checkAndGetAreaTypesExist(Collections.singletonList(areaTypeCode), validation);
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        AreaType areaType = areaTypes.get(0);

        // 3.
        AddressAllocationOrders order = addressAllocationOrderRepository.findById(orderId).orElse(null);

        if (order == null || Boolean.TRUE.equals(order.getArchived())) {
            throw new ContingentException(AreaErrorReason.ADDRESS_ALLOCATION_ORDER_NOT_EXISTS,
                    new ValidationParameter("orderId", orderId));
        }

        addressesRegistry = areaHelper.filterDistinctAddressesByGlobalId(addressesRegistry);
        // 4.
        algorithms.checkAddressFLK(addressesRegistry, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        // 5.
        addressesRegistry.forEach(addr -> {
            MoAddress moAddress = algorithms.searchServiceDistrictMOByAddress(areaType, addr, validation);
            if (moAddress != null) {
                validation.error(AreaErrorReason.ADDRESS_ALREADY_EXISTS,
                        new ValidationParameter("address", addr.getAddressString()),
                        new ValidationParameter("moId", moAddress.getMoId()));
            }
        });
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        // 6.
        List<Addresses> addresses = areaHelper.getMoAreaAddresses(addressesRegistry.stream().map(ar -> mappingDomainService.dtoToEntityTransform(ar)).collect(Collectors.toList()));
        // 7.
        List<MoAddress> moAddresses = new ArrayList<>();
        addresses.forEach(a -> {
            MoAddress moAddress = new MoAddress();
            moAddress.setAddress(a);
            moAddress.setAreaType(areaType);
            moAddress.setMoId(moId);
            moAddress.setAddressAllocationOrder(order);
            moAddress.setStartDate(LocalDate.now());
            moAddress.setCreateDate(LocalDateTime.now());
            moAddresses.add(moAddress);
        });
        moAddressRepository.saveAll(moAddresses);

        // Логирование добавление адресов
        moAddresses.forEach((moAddress) -> {
            historyHelper.sendHistory(null, moAddress, MoAddress.class);
        });

        return moAddresses.stream().map(MoAddress::getId).collect(Collectors.toList());
    }

    @Override
    public Long getNewAreaId() throws ContingentException {
        return areaRepository.getNextAreaId();
    }

    @Override
    public Page<AreaInfo> searchArea(Long areaTypeClassCode, Long moId, List<Long> muIds, List<Long> areaTypeCodes, Integer number, String description, Boolean isArchived, List<MedicalEmployee> medicalEmployees, List<SearchAreaAddress> searchAreaAddresses, Boolean isExactAddressMatch, PageRequest paging) throws ContingentException {

        //2
        areaHelper.checkSearchParameters(areaTypeClassCode, moId, muIds, areaTypeCodes, number, description,
                isArchived, medicalEmployees, searchAreaAddresses);

        //3
        areaHelper.checkSearchAreaInaccurateAddress(isExactAddressMatch, searchAreaAddresses);

        //4
        areaHelper.checkSearchAreaAddresses(searchAreaAddresses);

        //4.1
        List<Area> areas = areaRepository.findAreas(areaTypeClassCode, moId, muIds, areaTypeCodes, number, description, isArchived);

        //4.2
        if (!medicalEmployees.isEmpty()) {
            areas = areaMedicalEmployeeRepository.findAreas(areas.stream().map(Area::getId).collect(Collectors.toList()),
                    medicalEmployees.stream()
                            .map(MedicalEmployee::getMedicalEmployeeJobId)
                            .filter(Objects::nonNull)
                            .collect(Collectors.toList()),
                    medicalEmployees.stream()
                            .map(MedicalEmployee::getSnils)
                            .filter(Objects::nonNull)
                            .collect(Collectors.toList()));
        }

        //4.3
        if (!searchAreaAddresses.isEmpty()) {
            List<Addresses> addresses;
            List<AreaAddress> areaAddresses;
            //4.3.2
            if (isExactAddressMatch == null || isExactAddressMatch) {
                addresses = addressesRepository.findAddresses(searchAreaAddresses.stream()
                        .map(SearchAreaAddress::getGlobalIdNsi).collect(Collectors.toList()));
                //4.3.3
            } else {
                addresses = algorithms.findIntersectingAddressesSearch(searchAreaAddresses);
            }
            //4.3.4
            if (!addresses.isEmpty()) {
                areaAddresses = areaAddressRepository.findAreaAddressByAddressIds(addresses.stream().map(Addresses::getId).collect(Collectors.toList()));
                List<Long> areaIds = areaAddresses.stream().map(areaAddress -> areaAddress.getArea().getId()).collect(Collectors.toList());
                areas = areas.stream().filter(area -> areaIds.contains(area.getId())).collect(Collectors.toList());
            } else {
                areas = Collections.emptyList();
            }
        }

        int totalSize = areas.size();
        List<AreaInfo> areaInfos = areas.stream().sorted(Comparator.comparingLong(Area::getId))
                .skip(paging.getPageNumber() * paging.getPageSize()).limit(paging.getPageSize())
                .map(AreaInfo::new).collect(Collectors.toList());

        Map<Area, List<AreaMedicalEmployees>> mapMedicalEmployees = areaMedicalEmployeeRepository.getEmployeesByAreaIds(areaInfos.stream().map(AreaInfo::getArea).collect(Collectors.toList()));

        areaInfos.forEach(ai -> {
            // Добавление работников
            if (mapMedicalEmployees.get(ai.getArea()) != null) {
                List<AreaMedicalEmployees> mainMedicalEmployees = mapMedicalEmployees.get(ai.getArea()).stream()
                        .filter(me -> Boolean.valueOf(false).equals(me.getReplacement()))
                        .collect(Collectors.toList());
                ai.setMainAreaMedicalEmployees(mainMedicalEmployees);

                // 3.
                List<AreaMedicalEmployees> replacementMedicalEmployees = mapMedicalEmployees.get(ai.getArea()).stream()
                        .filter(me -> Boolean.valueOf(true).equals(me.getReplacement()))
                        .collect(Collectors.toList());

                ai.setReplacementAreaMedicalEmployees(replacementMedicalEmployees);
            }

        });

        return new PageImpl<>(new ArrayList<>(areaInfos),
                paging, totalSize);
    }

    @Override
    public Long initiateCreatePrimaryArea(long moId, Long muId, Integer number, String description, Long areaTypeCode,
                                          List<Long> policyTypes, Integer ageMin, Integer ageMax, Integer ageMinM,
                                          Integer ageMaxM, Integer ageMinW, Integer ageMaxW,
                                          boolean autoAssignForAttachment, Boolean attachByMedicalReason,
                                          List<AddMedicalEmployee> addMedicalEmployees,
                                          List<AddressRegistry> addresses) throws ContingentException {

        // 2
        long sysopId = algorithms.sysOperationRegistration(SysopMethodType.INITIATE_CREATE_PRIMARY_AREA);

        // 3
        areaServiceInternalAsync.asyncCreatePrimaryArea(UserContextHolder.getContext(), sysopId, moId, muId, number, description,
                areaTypeCode, policyTypes, ageMin, ageMax, ageMinM, ageMaxM, ageMinW, ageMaxW,
                autoAssignForAttachment, attachByMedicalReason, addMedicalEmployees, addresses);

        // 4 выполняется автоматически после выполнения createPrimaryArea, setMedicalEmployeeOnArea, addAreaAddress

        // 5
        return sysopId;
    }

    @Override
    public Long initiateAddMoAddress(long moId, long areaTypeCode, long orderId, List<AddressRegistry> addresses) throws ContingentException {
        //1. Система выполняет проверку полномочий пользователя.
        // Реализовано через аннотацию

        //2. Система выполняет регистрацию новой асинхронной операции
        long sysopId = algorithms.sysOperationRegistration(SysopMethodType.INITIATE_ADD_MO_ADDRESS);

        //3. Система инициирует процесс (выполняется асинхронно) распределения жилых домов к территории обслуживания МО. (А_УУ_11)
        areaServiceInternalAsync.asyncInitiateAddMoAddress(sysopId, UserContextHolder.getContext(), moId, areaTypeCode, orderId, addresses);

        //4. Система инициирует процесс журналирования (выполняется асинхронно) по инициации распределения жилых домов к территории обслуживания МО. (А_УУ_8), п.1
        // происходит в п 3 (не забываем добавлять имя метода в AreaServiceLogMethodsEnum)

        //5. Система возвращает в качестве результата: ИД операции
        return sysopId;
    }

    @Override
    public Long initiateAddAreaAddress(Long areaId, List<AddressRegistry> addressesRegistry) throws ContingentException {
        //1. Система выполняет проверку полномочий пользователя.
        // Реализовано через аннотацию

        // 2. Система выполняет регистрацию новой асинхронной операции
        long sysopId = algorithms.sysOperationRegistration(SysopMethodType.INITIATE_ADD_AREA_ADDRESS);

        // 3. Система инициирует процесс (выполняется асинхронно) добавления адресов на участок обслуживания.
        areaServiceInternalAsync.asyncAddAreaAddress(UserContextHolder.getContext(), sysopId, areaId, addressesRegistry);

        // 4. Система инициирует процесс журналирования (выполняется асинхронно) по инициации добавления адресов на участок обслуживания.
        // выполняется в п 3 (не забываем добавлять имя метода в AreaServiceLogMethodsEnum)

        // 5. Система возвращает в качестве результата: ИД операции
        return sysopId;
    }
    @Override
    public Page<Area> searchDnArea(Long moId, List<Long> muIds, List<Long> areaTypeCodes, List<Long> specializationCodes, List<Long> areaIds, PageRequest paging) throws ContingentException {
        //2
        areaHelper.checkSearchDnParameters(moId, muIds, areaTypeCodes, specializationCodes, areaIds);
        //3, 4, 5
        return areaRepository.findAreas(moId, muIds, areaTypeCodes, specializationCodes, areaIds, paging);

    }

    @Override
    public Page<Area> getAreaListBrief(List<Long> areaIds, PageRequest paging) throws ContingentException {
        Long maxIds = settingService.getSettingProperty(SettingService.MAX_AREA_IDS_FOR_SEARCH);

        Validation validation = new Validation();

        areaHelper.checkMaxPage(paging, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        if (maxIds != null && areaIds.size() > maxIds) {
            validation.error(AreaErrorReason.MAX_AREA_IDS_EXCEEDED, new ValidationParameter("areas", maxIds));
        }
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        if (paging.getSort().isUnsorted()) {
            //Если блок sortOrder не передан, сортировка результатов осуществляется по ИД участка (id) по возрастанию
            paging = PageRequest.of(paging.getPageNumber(), paging.getPageSize(), Sort.by(Area_.id.getName()));
        }
        return areaRepository.getAreas(areaIds, paging);
    }

    @Override
    public Page<Area> searchMuByAreaAddress(List<Long> areaTypeCodes, String areaOMKTECode, String regionOMKTECode,
                                     PageRequest paging) throws ContingentException {

        Validation validation = new Validation();

        areaHelper.checkMaxPage(paging, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        List<Addresses> addresses;

        if (StringUtils.hasText(regionOMKTECode)) {
            // 4.1.2 Система выполняет поиск адреса из входных параметров, а также адресов более низких уровней в таблице Адрес (ADDRESSES)
            addresses = addressesRepository.findAddresses(null, regionOMKTECode, null);
        }
        else {
            // 4.2.2 Система выполняет поиск адресов в таблице Адрес (ADDRESSES)
            String regionTeCode = areaOMKTECode.substring(0, 2) + "00";
            // поиск адреса из входных параметров, а также адресов более низких уровней, т.е. всех записей, у которых
            addresses = addressesRepository.findAddresses(areaOMKTECode, null, null);
            // поиск адреса округа, к которому относится данный район
            addresses.addAll(addressesRepository.findAddresses(null, regionTeCode, AddressLevelType.REGION_TE.getLevel()));
        }
        if (addresses.isEmpty()) {
            throw new ContingentException(new Validation().error(AreaErrorReason.ADDRESS_NOT_FOUND));
        }
        // 5. Система выполняет поиск актуальных участков заданного типа, обслуживающих адреса, полученные на предыдущем этапе сценария,
        // и получает их ИД МУ (AREAS.MU_ID) и ИД МО (AREAS.MO_ID)
        return areaRepository.findActualAreasByAddressIds(areaTypeCodes, addresses.stream().map(Addresses::getId).collect(Collectors.toList()), paging);
    }

    @Override
    public Page<Area> searchMuByAreaAddress(List<Long> areaTypeCodes, String aoLevel, long globalIdNsi,
                                     PageRequest paging) throws ContingentException {
        // 3.1 Система выполняет поиск адреса из входных параметров в таблице Адрес
        List<Addresses> addresses = addressesRepository.findAddresses(Collections.singletonList(globalIdNsi), aoLevel);

        if (addresses.isEmpty()) {
            throw new ContingentException(new Validation().error(AreaErrorReason.ADDRESS_NOT_FOUND));
        }
        if (addresses.size() > 1) {
            throw new ContingentException(new Validation().error(AreaErrorReason.MULTIPLE_NSI_ADDRESSES_ERROR));
        }
        // 3.2 Система выполняет поиск адресов, пересекающихся с адресом, полученным на предыдущем шаге
        addresses.addAll(algorithms.findIntersectingAddressesSearch(Collections.singletonList(new SearchAreaAddress(addresses.get(0)))));
        // 5. Система выполняет поиск актуальных участков заданного типа, обслуживающих адреса, полученные на предыдущем этапе сценария,
        // и получает их ИД МУ (AREAS.MU_ID) и ИД МО (AREAS.MO_ID)
        return areaRepository.findActualAreasByAddressIds(areaTypeCodes, addresses.stream().map(Addresses::getId).collect(Collectors.toList()), paging);
    }

    @Override
    public void editAddress(Long arGlobalId, Map<String, String> fields) throws ContingentException {
        List<String> doNotUpdateFields = Arrays.asList("ID", "CREATE_DATE", "UPDATE_DATE", "GLOBAL_ID");
        //1.
        if (fields.keySet().stream().anyMatch(doNotUpdateFields::contains)) {
            throw new ContingentException(new Validation().error(AreaErrorReason.FORBIDDEN_ADDRESS_UPDATE_FIELDS,
                    new ValidationParameter("options", String.join("; ", doNotUpdateFields))));
        }
        //2.
        Addresses address = addressesRepository.findAddressByGlobalId(arGlobalId);

        if (address == null) {
            throw new ContingentException(new Validation().error(AreaErrorReason.NSI_ADDRESS_NOT_FOUND,
                    new ValidationParameter("arGlobalId", String.valueOf(arGlobalId))));
        }
        List<String> notFoundFields = new ArrayList<>();
        Map<Field, String> fieldValues = new HashMap<>();
        //3.
        for (Map.Entry<String, String> entry : fields.entrySet()) {
            Field field = areaHelper.findTableField(entry.getKey(), address);

            if (field == null) {
                notFoundFields.add(entry.getKey());
            }
            else {
                fieldValues.put(field, entry.getValue());
            }
        }
        if (!notFoundFields.isEmpty()) {
            throw new ContingentException(new Validation().error(AreaErrorReason.WRONG_ADDRESS_UPDATE_FIELDS,
                    new ValidationParameter("options", String.join("; ", notFoundFields))));
        }
        //4.
        for (Map.Entry<Field, String> entry : fieldValues.entrySet()) {
            try {
                //Если "input value" = "null" (во входных параметрах значение передается без кавычек), то Система обнуляет значение соответствующего атрибута в таблице ADDRESSES
                Object value = "null".equals(entry.getValue()) ? null : EntityConverterHelper.parseValue(entry.getValue(), entry.getKey().getType());
                Method fieldSetter = EntityConverterHelper.getSetterMethod(Addresses.class, entry.getKey());
                fieldSetter.invoke(address, value);
            }
            catch (IllegalAccessException | InvocationTargetException ex) {
                throw new RuntimeException("Не удалось применить значение поля " + entry.getKey().getName(), ex);
            }
        }
        address.setUpdateDate(LocalDateTime.now());
    }
}
