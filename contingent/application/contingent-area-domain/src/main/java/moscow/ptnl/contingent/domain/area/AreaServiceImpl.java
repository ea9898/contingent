package moscow.ptnl.contingent.domain.area;


import moscow.ptnl.contingent.domain.AreaErrorReason;
import moscow.ptnl.contingent.domain.PageImplCustom;
import moscow.ptnl.contingent.domain.area.entity.AddressAllocationOrders;
import moscow.ptnl.contingent.domain.area.entity.Addresses;
import moscow.ptnl.contingent.domain.area.entity.Area;
import moscow.ptnl.contingent.domain.area.entity.AreaAddress;
import moscow.ptnl.contingent.domain.area.entity.AreaMedicalEmployees;
import moscow.ptnl.contingent.domain.area.entity.AreaMuService;
import moscow.ptnl.contingent.domain.area.entity.AreaPolicyTypes;
import moscow.ptnl.contingent.domain.area.entity.AreaToAreaType;
import moscow.ptnl.contingent.domain.area.entity.Area_;
import moscow.ptnl.contingent.domain.area.entity.MoAddress;
import moscow.ptnl.contingent.domain.area.heplers.AreaHelper;
import moscow.ptnl.contingent.domain.area.heplers.MedicalEmployeeHelper;
import moscow.ptnl.contingent.domain.area.model.area.AddMedicalEmployee;
import moscow.ptnl.contingent.domain.area.model.area.AddressLevelType;
import moscow.ptnl.contingent.domain.area.model.area.AddressRegistry;
import moscow.ptnl.contingent.domain.area.model.area.AreaHistory;
import moscow.ptnl.contingent.domain.area.model.area.AreaInfo;
import moscow.ptnl.contingent.domain.area.model.area.AreaOrEmployeeEvent;
import moscow.ptnl.contingent.domain.area.model.area.ChangeMedicalEmployee;
import moscow.ptnl.contingent.domain.area.model.area.MedicalEmployee;
import moscow.ptnl.contingent.domain.area.model.area.MoAddressAllocation;
import moscow.ptnl.contingent.domain.area.model.area.MoMuPair;
import moscow.ptnl.contingent.domain.area.model.area.SearchAreaAddress;
import moscow.ptnl.contingent.domain.area.model.sysop.SysopMethodType;
import moscow.ptnl.contingent.domain.area.repository.AddressAllocationOrderRepository;
import moscow.ptnl.contingent.domain.area.repository.AddressesRepository;
import moscow.ptnl.contingent.domain.area.repository.AreaAddressRepository;
import moscow.ptnl.contingent.domain.area.repository.AreaMedicalEmployeeRepository;
import moscow.ptnl.contingent.domain.area.repository.AreaMuServiceRepository;
import moscow.ptnl.contingent.domain.area.repository.AreaPolicyTypesRepository;
import moscow.ptnl.contingent.domain.area.repository.AreaRepository;
import moscow.ptnl.contingent.domain.area.repository.AreaToAreaTypeRepository;
import moscow.ptnl.contingent.domain.area.repository.HistoryEventRepository;
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
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeProfile;
import moscow.ptnl.contingent.nsi.domain.area.PolicyType;
import moscow.ptnl.contingent.nsi.domain.repository.AreaTypeMedicalPositionsRepository;
import moscow.ptnl.contingent.nsi.domain.repository.AreaTypeSpecializationsRepository;
import moscow.ptnl.contingent.nsi.domain.repository.AreaTypesRepository;
import moscow.ptnl.contingent.nsi.domain.repository.MappingPositionCodeToOtherPositionRepository;
import moscow.ptnl.contingent.nsi.domain.repository.PolicyTypeRepository;
import moscow.ptnl.contingent.nsi.domain.repository.PositionCodeRepository;
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
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;
import reactor.util.function.Tuple2;
import reactor.util.function.Tuples;

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
    private MedicalEmployeeHelper medicalEmployeeHelper;

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

    @Autowired
    private AreaMuServiceRepository areaMuServiceRepository;

    @Autowired
    private HistoryEventRepository historyEventRepository;

    @Autowired
    private PositionCodeRepository positionCodeRepository;

    @Autowired
    private MappingPositionCodeToOtherPositionRepository mappingPositionCodeToOtherPositionRepository;

    @Override @LogESU(type = AreaInfoEvent.class, useResult = true)
    public Long createPrimaryArea(long moId, Long muId, Integer number, Long areaTypeCode, List<Long> policyTypesIds,
                                  Integer ageMin, Integer ageMax, Integer ageMinM, Integer ageMaxM, Integer ageMinW, Integer ageMaxW,
                                  boolean autoAssignForAttachment, Boolean attachByMedicalReason, String description) throws ContingentException {
        return createPrimaryAreaInternal(moId, muId, number, areaTypeCode, null, policyTypesIds, ageMin, ageMax, ageMinM, ageMaxM, ageMinW, ageMaxW, autoAssignForAttachment, attachByMedicalReason, description).getId();
    }

    @Override @LogESU(type = AreaInfoEvent.class, useResult = true)
    public Long createPrimaryArea(long moId, Long muId, Integer number, Long areaTypeCode, Long areaTypeProfileCode, List<Long> policyTypesIds,
                                  Integer ageMin, Integer ageMax, Integer ageMinM, Integer ageMaxM, Integer ageMinW, Integer ageMaxW,
                                  boolean autoAssignForAttachment, Boolean attachByMedicalReason, String description) throws ContingentException {
        return createPrimaryAreaInternal(moId, muId, number, areaTypeCode, areaTypeProfileCode, policyTypesIds, ageMin, ageMax, ageMinM, ageMaxM, ageMinW, ageMaxW, autoAssignForAttachment, attachByMedicalReason, description).getId();
    }

    @Override
    public Area createPrimaryAreaInternal(long moId, Long muId, Integer number, Long areaTypeCode, Long areaTypeProfileCode, List<Long> policyTypesIds,
                                          Integer ageMin, Integer ageMax, Integer ageMinM, Integer ageMaxM, Integer ageMinW, Integer ageMaxW,
                                          boolean autoAssignForAttachment, Boolean attachByMedicalReason, String description) throws ContingentException {
        Validation validation = new Validation();

        // 1
        List<AreaType> areaTypeList = areaHelper.checkAndGetAreaTypesExist(Collections.singletonList(areaTypeCode), validation);

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

        List<AreaTypeProfile> areaTypeProfiles = Collections.emptyList();

        if (UserContextHolder.getContractVersion() == 2) {
            // 12
            if (settingService.par39().contains(areaTypeCode)) {
                if (areaTypeProfileCode == null) {
                    validation.error(AreaErrorReason.EMPTY_AREA_TYPE_PROFILE, new ValidationParameter("areaTypeTitle", areaType.getTitle()));
                } else {
                    areaTypeProfiles = areaHelper.checkAndGetAreaTypeProfiles(Collections.singletonList(areaTypeProfileCode), areaType, validation);
                }
            }
        }
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        // 13
        Area area = Area.builder()
                .moId(moId)
                .muId(muId)
                .areaType(areaType)
                .areaTypeProfile(areaTypeProfiles.isEmpty() ? null : areaTypeProfiles.get(0))
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

        // 14
        List<PolicyType> policyTypes = policyTypeRepository.findByIds(policyTypesIds);
        List<AreaPolicyTypes> areaPolicyTypes = policyTypes.stream().map(policyType ->
                new AreaPolicyTypes(area, policyType)).collect(Collectors.toList());
        areaPolicyTypesRepository.saveAll(areaPolicyTypes);

        // 16
        historyHelper.sendHistory(null, area, Area.class);

        return area;
    }

    @Override
    public Long createDependentArea(long moId, Long muId, Integer number, Long areaTypeCode, Long areaTypeProfileCode, List<Long> primaryAreaTypeCodesIds,
                                    List<Long> policyTypeCodesIds, Integer ageMin, Integer ageMax, Integer ageMinM, Integer ageMaxM, Integer ageMinW, Integer ageMaxW,
                                    String description) throws ContingentException {

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

        List<AreaTypeProfile> areaTypeProfiles = Collections.emptyList();

        if (UserContextHolder.getContractVersion() == 2) {
            //11
            if (settingService.par39().contains(areaTypeCode)) {
                if (areaTypeProfileCode == null) {
                    validation.error(AreaErrorReason.EMPTY_AREA_TYPE_PROFILE, new ValidationParameter("areaTypeTitle", areaType.getTitle()));
                } else {
                    areaTypeProfiles = areaHelper.checkAndGetAreaTypeProfiles(Collections.singletonList(areaTypeProfileCode), areaType, validation);
                }
            }
        }
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        //12
        Area area = Area.builder()
                .moId(moId)
                .muId(muId)
                .areaType(areaType)
                .areaTypeProfile(areaTypeProfiles.isEmpty() ? null : areaTypeProfiles.get(0))
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

        //13
        for (Long areaTypeCodeId : primaryAreaTypeCodesIds) {
            AreaToAreaType areaToAreaType = new AreaToAreaType();
            areaToAreaType.setArea(area);
            areaToAreaType.setAreaType(areaTypesRepository.findById(areaTypeCodeId).get());
            areaToAreaTypeRepository.save(areaToAreaType);
        }
        //14
        List<PolicyType> policyTypeList = policyTypeRepository.findByIds(policyTypeCodesIds);

        if (policyTypeCodesIds != null && !policyTypeCodesIds.isEmpty()) {
            AreaPolicyTypes areaPolicyTypes = new AreaPolicyTypes();
            areaPolicyTypes.setArea(area);
            areaPolicyTypes.setPolicyType(policyTypeList.get(0));
            areaPolicyTypesRepository.save(areaPolicyTypes);
        }

        //15
        List<Area> primAreas = areaRepository.findAreas(moId, muId, primaryAreaTypeCodesIds, null, true);

        //16
        if (primAreas != null && !primAreas.isEmpty()) {
            esuHelperService.sendAttachOnAreaChangeEvent(primAreas.stream().map(Area::getId).collect(Collectors.toList()),
                    null, area);
        }

        //17
        historyHelper.sendHistory(null, area, Area.class);

        //18
        return area.getId();
    }

    @Override @LogESU(type = AreaInfoEvent.class, parameters = {"areaId"})
    public void updatePrimaryArea(long areaId, Integer number, List<Long> policyTypesAddIds, List<Long> policyTypesDelIds, Integer ageMin, Integer ageMax, Integer ageMinM, Integer ageMaxM, Integer ageMinW, Integer ageMaxW, Boolean autoAssignForAttachment, Boolean attachByMedicalReason, String description) throws ContingentException {
        Validation validation = new Validation();

        // 1, 2
        Area area = areaHelper.checkAndGetArea(areaId, validation, true);
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
        Area area = areaHelper.checkAndGetArea(areaId, validation, true);

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
        Area area = areaHelper.checkAndGetArea(areaId, validation, true);

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

        //5
        for (ChangeMedicalEmployee inputEmployee : changeEmployeesInput) {
            medicalEmployeeHelper.checkChangeMedicalEmployee(inputEmployee, areaId, areaEmployeesDb, validation);
        }
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        //6
        for (AddMedicalEmployee empl : addEmployeesInput) {
            medicalEmployeeHelper.checkAddMedicalEmployee(empl, area.getAreaType(), validation);
        }
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        List<ChangeMedicalEmployee> changeEmployeesCorrectInput = changeEmployeesInput.stream()
                .filter(e -> !Boolean.TRUE.equals(e.isIsError()))
                .collect(Collectors.toList());
        Set<Long> changedEmployeeIds = new HashSet<>(changeEmployeesCorrectInput.stream()
                .map(ChangeMedicalEmployee::getAssignmentId)
                .collect(Collectors.toList()));
        //7.1
        List<AreaMedicalEmployees> allEmployees = new ArrayList<>(areaEmployeesDb);
        Map<AreaMedicalEmployees, AreaMedicalEmployees> changesAme =  areaHelper.applyChanges(allEmployees, changeEmployeesCorrectInput);
        areaHelper.addNew(allEmployees, addEmployeesInput, area);
        areaHelper.checkDatesNotInterceptWithSamePosition(
                allEmployees.stream().filter(me -> me.getError() == null || !me.getError()).collect(Collectors.toList()), validation); // отфитровываем ошибочно назначенные работников из проверки

        //7.2
        List<AreaMedicalEmployees> mainEmployees =
                areaEmployeesDb.stream().filter(empl -> !empl.getReplacement()).collect(Collectors.toList());
        areaHelper.applyChanges(mainEmployees, changeEmployeesCorrectInput);
        areaHelper.addNew(mainEmployees, addEmployeesInput.stream()
                .filter(empl -> !empl.isReplacement()).collect(Collectors.toList()), area);
        if (area.getAreaType() != null && area.getAreaType().getAreaTypeKind() != null &&
                area.getAreaType().getAreaTypeKind().getCode() == AreaTypeKindEnum.MILDLY_ASSOCIATED.getCode()) {
            areaHelper.checkMainEmployeesOverlappingDates(
                    mainEmployees.stream().filter(me -> me.getError() == null || !me.getError()).collect(Collectors.toList()), validation);
            //7.3
            areaHelper.checkMainEmployeesUniqueness(area, mainEmployees, changedEmployeeIds, validation);
            //7.4
            areaHelper.checkTempDutyEmployees(changeEmployeesCorrectInput, addEmployeesInput, areaEmployeesDb, validation);
            //7.5
            areaHelper.checkTempDutyEmployeesUniqueness(area, allEmployees, changedEmployeeIds, validation);
        }
        //7.6
        if (area.getAreaType() != null &&
                area.getAreaType().getAreaTypeKind() != null &&
                (area.getAreaType().getAreaTypeKind().getCode().equals(AreaTypeKindEnum.MILDLY_ASSOCIATED.getCode()) ||
                        area.getAreaType().getAreaTypeKind().getCode().equals(AreaTypeKindEnum.TREATMENT_ROOM_ASSOCIATED.getCode()))) {
            //7.6.1.
            List<Period> periodsWithoutMainEmpl = areaHelper.getPeriodsWithoutMainEmployee(mainEmployees);
            if (periodsWithoutMainEmpl.size() > 0) {
                //7.6.2.
                List<AreaMedicalEmployees> replacementEmployees = areaEmployeesDb.stream()
                        .filter(AreaMedicalEmployees::getReplacement).collect(Collectors.toList());
                areaHelper.applyChanges(replacementEmployees, changeEmployeesCorrectInput);
                areaHelper.addNew(replacementEmployees, addEmployeesInput.stream()
                        .filter(AddMedicalEmployee::isReplacement).collect(Collectors.toList()), area);
                replacementEmployees.sort(Comparator.comparing(AreaMedicalEmployees::getStartDate, nullsFirst(naturalOrder())));
//                areaHelper.checkReplacementWithoutMain(periodsWithoutMainEmpl, replacementEmployees, validation); https://jira.emias.mos.ru/browse/CONTINGENT2-643
            }
        }

        //8
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
        areaInfo.setAreaServicedMUs(new ArrayList<>(areaInfo.getArea().getActualAreaMuServices()));

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
        Area area = areaHelper.checkAndGetArea(areaId, validation, true);
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
        Area area = areaHelper.checkAndGetArea(areaId, validation, true);
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
        Area area = areaHelper.checkAndGetArea(areaId, validation, true);

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
    public List<Long> addMoAddress(long moId, List<Long> areaTypeCodes, long orderId, List<AddressRegistry> addressesRegistry, boolean limitAddress) throws ContingentException {
        Validation validation = new Validation();
        // 1.
        if (limitAddress) {
            areaHelper.checkTooManyAddresses(addressesRegistry, settingService.getPar1());
        }

        // 2.
        List<AreaType> areaTypes = areaHelper.checkAndGetAreaTypesExist(areaTypeCodes, validation);
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

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
        for (AreaType areaType : areaTypes) {
            addressesRegistry.forEach(addr -> {
                MoAddress moAddress = algorithms.searchServiceDistrictMOByAddress(areaType, addr, validation);

                if (moAddress != null) {
                    validation.error(AreaErrorReason.ADDRESS_ALREADY_EXISTS,
                            new ValidationParameter("address", addr.getAddressString()),
                            new ValidationParameter("moId", moAddress.getMoId()));
                }
            });
        }
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        // 6.
        List<Addresses> addresses = areaHelper.getMoAreaAddresses(addressesRegistry.stream().map(ar -> mappingDomainService.dtoToEntityTransform(ar)).collect(Collectors.toList()));
        // 7.
        List<MoAddress> moAddresses = new ArrayList<>();

        for (AreaType areaType : areaTypes) {
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
        }
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
    public Page<AreaInfo> searchArea(Long areaTypeClassCode, Long moId, List<Long> muIds, List<Long> areaTypeCodes, Long areaTypeProfile,
                                     List<Long> servicedMuIds, Integer number, String description, Boolean isArchived, List<MedicalEmployee> medicalEmployees,
                                     List<SearchAreaAddress> searchAreaAddresses, Boolean isExactAddressMatch, PageRequest paging,
                                     boolean loadServicedMUs) throws ContingentException {
        //2
        if (UserContextHolder.getContractVersion() > 1) {
            areaHelper.checkSearchParametersV2(areaTypeClassCode, moId, muIds, areaTypeCodes, areaTypeProfile, servicedMuIds,
                    number, description, isArchived, medicalEmployees, searchAreaAddresses);
        } else {
            areaHelper.checkSearchParameters(areaTypeClassCode, moId, muIds, areaTypeCodes, number, description,
                    isArchived, medicalEmployees, searchAreaAddresses);
        }
        //3
        areaHelper.checkSearchAreaInaccurateAddress(isExactAddressMatch, searchAreaAddresses);

        //4
        areaHelper.checkSearchAreaAddresses(searchAreaAddresses);

        //5 Система выполняет поиск участков по переданным входным параметрам (логическое И шагов 5.1, 5.2, 5.3, 5.4):
        //5.1
        List<Area> areas = null;

        boolean noSearchAreas = StringUtils.isEmpty(areaTypeClassCode) && StringUtils.isEmpty(moId) && CollectionUtils.isEmpty(muIds) &&
                CollectionUtils.isEmpty(areaTypeCodes) && areaTypeProfile == null && CollectionUtils.isEmpty(servicedMuIds) &&
                StringUtils.isEmpty(number) && StringUtils.isEmpty(description) && isArchived == null;

        if (!noSearchAreas) {
            //Только если есть критерии поиска для запроса
            areas = areaRepository.findAreas(areaTypeClassCode, moId, muIds, areaTypeCodes, areaTypeProfile,
                    servicedMuIds, number, description, isArchived);
        }
        //5.2
        if ((areas == null || !areas.isEmpty()) && !medicalEmployees.isEmpty()) {
            List<Long> jobIds = medicalEmployees.stream()
                    .map(MedicalEmployee::getMedicalEmployeeJobId)
                    .filter(Objects::nonNull)
                    .collect(Collectors.toList());
            List<String> snilsCodes = medicalEmployees.stream()
                    .map(MedicalEmployee::getSnils)
                    .filter(Objects::nonNull)
                    .collect(Collectors.toList());

            if (areas != null && areas.size() > 1000) {
                //Чтобы не передавать в SELECT .. IN () слишком много ID
                List<Long> foundAreaIds = areaMedicalEmployeeRepository.findAreas(null, jobIds, snilsCodes).stream()
                        .map(Area::getId)
                        .collect(Collectors.toList());
                areas = areas.stream().filter(area -> foundAreaIds.contains(area.getId())).collect(Collectors.toList());
            } else {
                areas = areaMedicalEmployeeRepository.findAreas(areas == null ? null : areas.stream().map(Area::getId).collect(Collectors.toList()),
                        jobIds, snilsCodes);
            }
        }

        //5.3
        if (!searchAreaAddresses.isEmpty()) {
            List<Addresses> addresses;
            List<AreaAddress> areaAddresses;
            //5.3.2
            if (isExactAddressMatch == null || isExactAddressMatch) {
                addresses = addressesRepository.findAddresses(searchAreaAddresses.stream()
                        .map(SearchAreaAddress::getGlobalIdNsi).collect(Collectors.toList()));
                //5.3.3
            } else {
                addresses = algorithms.findIntersectingAddressesSearch(searchAreaAddresses);
            }
            //5.3.4
            if (!addresses.isEmpty()) {
                areaAddresses = areaAddressRepository.findAreaAddressByAddressIds(addresses.stream().map(Addresses::getId).collect(Collectors.toList()));

                if (areas == null) {
                    areas = areaAddresses.stream().map(AreaAddress::getArea).collect(Collectors.toList());
                } else {
                    List<Long> areaIds = areaAddresses.stream().map(areaAddress -> areaAddress.getArea().getId()).collect(Collectors.toList());
                    areas = areas.stream().filter(area -> areaIds.contains(area.getId())).collect(Collectors.toList());
                }
            } else {
                areas = Collections.emptyList();
            }
        }

        int totalSize = areas.size();
        List<AreaInfo> areaInfos = areas.stream()
                .distinct()
                .sorted(Comparator.comparingLong(Area::getId))
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
        if (loadServicedMUs) {
            List<Long> areaIds = areaInfos.stream().map(AreaInfo::getArea).map(Area::getId).distinct().collect(Collectors.toList());

            if (!areaIds.isEmpty()) {
                Map<Area, List<AreaMuService>> muServices = areaMuServiceRepository.findActive(areaIds).stream()
                        .collect(Collectors.groupingBy(AreaMuService::getArea));
                areaInfos.forEach(i -> i.setAreaServicedMUs(muServices.getOrDefault(i.getArea(), Collections.emptyList())));
            }
        }
        return new PageImpl<>(new ArrayList<>(areaInfos), paging, totalSize);
    }

    @Override
    public Long initiateCreatePrimaryArea(long moId, Long muId, Integer number, String description, Long areaTypeCode, Long areaTypeProfileCode,
                                          List<Long> policyTypes, Integer ageMin, Integer ageMax, Integer ageMinM,
                                          Integer ageMaxM, Integer ageMinW, Integer ageMaxW,
                                          boolean autoAssignForAttachment, Boolean attachByMedicalReason,
                                          List<AddMedicalEmployee> addMedicalEmployees,
                                          List<AddressRegistry> addresses) throws ContingentException {

        // 2
        long sysopId = algorithms.sysOperationRegistration(SysopMethodType.INITIATE_CREATE_PRIMARY_AREA);

        // 3
        areaServiceInternalAsync.asyncCreatePrimaryArea(UserContextHolder.getContext(), sysopId, moId, muId, number, description,
                areaTypeCode, areaTypeProfileCode, policyTypes, ageMin, ageMax, ageMinM, ageMaxM, ageMinW, ageMaxW,
                autoAssignForAttachment, attachByMedicalReason, addMedicalEmployees, addresses);

        // 4 выполняется автоматически после выполнения createPrimaryArea, setMedicalEmployeeOnArea, addAreaAddress

        // 5
        return sysopId;
    }

    @Override
    public Long initiateAddMoAddress(long moId, List<Long> areaTypeCodes, long orderId, List<AddressRegistry> addresses) throws ContingentException {
        //1. Система выполняет проверку полномочий пользователя.
        // Реализовано через аннотацию

        //2. Система выполняет регистрацию новой асинхронной операции
        long sysopId = algorithms.sysOperationRegistration(SysopMethodType.INITIATE_ADD_MO_ADDRESS);

        //3. Система инициирует процесс (выполняется асинхронно) распределения жилых домов к территории обслуживания МО. (А_УУ_11)
        areaServiceInternalAsync.asyncInitiateAddMoAddress(sysopId, UserContextHolder.getContext(), moId, areaTypeCodes, orderId, addresses);

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
    public Page<AreaInfo> searchDnArea(Long moId, List<Long> muIds, List<Long> areaTypeCodes, Long areaTypeProfileCode, List<Long> servicedMuIds,
                                   List<String> specializationCodes, List<Long> areaIds, PageRequest paging, boolean loadServicedMUs) throws ContingentException {
        //2
        if (loadServicedMUs) { //V2
            areaHelper.checkSearchDnParameters(moId, muIds, areaTypeCodes, areaTypeProfileCode, servicedMuIds, specializationCodes, areaIds);
        } else { //V1
            areaHelper.checkSearchDnParameters(moId, muIds, areaTypeCodes, specializationCodes, areaIds);
        }
        //3, 4, 5
        Page<Area> areas = areaRepository.findAreas(moId, muIds, areaTypeCodes, areaTypeProfileCode, servicedMuIds, specializationCodes, areaIds, paging);

        List<AreaInfo> areaInfos = areas.stream()
                .map(a -> new AreaInfo(a, new ArrayList<>(a.getActualMainMedicalEmployees()),
                        new ArrayList<>(a.getActualReplacementMedicalEmployees()),
                        loadServicedMUs ? new ArrayList<>(a.getActualAreaMuServices()) : null))
                .collect(Collectors.toList());
        return new PageImpl<>(new ArrayList<>(areaInfos), paging, areas.getTotalElements());
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
    public Page<AreaInfo> getAreaListBriefV2(List<Long> areaIds, Boolean fetchMedicalEmployees, PageRequest paging) throws ContingentException {
        Page<Area> areas = getAreaListBrief(areaIds, paging);

        return new PageImpl<>(areas.stream().map(a -> {
            Set<AreaMedicalEmployees> employees = Collections.emptySet();

            if (fetchMedicalEmployees == null) {
                employees = a.getActualMainMedicalEmployees();
            } else if (Boolean.TRUE.equals(fetchMedicalEmployees)) {
                employees = a.getActualMedicalEmployees();
            }
            return new AreaInfo(a, new ArrayList<>(employees), null, new ArrayList<>(a.getActualAreaMuServices()));
        }).collect(Collectors.toList()), paging, areas.getTotalElements());
    }

    @Override
    public Page<MoMuPair> searchMuByAreaAddress(List<Long> areaTypeCodes, String areaOMKTECode, String regionOMKTECode,
                                                PageRequest paging) throws ContingentException {
        Validation validation = new Validation();

        areaHelper.checkMaxPage(paging, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        areaTypeCodes = areaTypeCodes == null || areaTypeCodes.isEmpty() ? null : areaTypeCodes;
        regionOMKTECode = StringUtils.hasText(regionOMKTECode) ? regionOMKTECode : null;
        String regionTeCode = areaOMKTECode == null ? null : areaOMKTECode.substring(0, 2) + "00";

        Page<MoMuPair> results = areaAddressRepository.findMoMuList(areaTypeCodes, areaOMKTECode, regionOMKTECode,
                regionTeCode, AddressLevelType.REGION_TE.getLevel(), LocalDate.now(), paging);

        if (results.isEmpty()) {
            throw new ContingentException(new Validation().error(AreaErrorReason.ADDRESS_NOT_FOUND));
        }
        return results;
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

    @Override @LogESU(type = AreaInfoEvent.class, parameters = {"areaId"})
    public void setAreaMuService(long areaId, List<Long> addServicedMuIds, List<Long> closeServicedMuIds) throws ContingentException {
        Validation validation = new Validation();
        //2, 3
        Area area = areaHelper.checkAndGetArea(areaId, validation, true);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        //4
        if (area.getAreaType() == null || !settingService.par40().contains(area.getAreaType().getCode())) {
            validation.error(AreaErrorReason.CANT_ADD_SERVICED_MU, new ValidationParameter("areaTypeTitle",
                    area.getAreaType() == null ? "" : area.getAreaType().getTitle()));
        }
        //5
        if (addServicedMuIds.isEmpty() && closeServicedMuIds.isEmpty()) {
            validation.error(AreaErrorReason.NOTHING_TO_CHANGE);
        }
        Area oldArea = historyHelper.clone(area);
        //6
        areaHelper.checkMuAlreadyServiced(area, addServicedMuIds, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        //6.1, 7.1
        List<AreaMuService> areaMuServicesAdded = new ArrayList<>();

        for (Long servicedMuId : addServicedMuIds) {
            if (areaMuServiceRepository.findActive(servicedMuId, areaId).isEmpty()) {
                areaMuServicesAdded.add(AreaMuService.buildActive(servicedMuId, area));
            }
        }
        areaMuServiceRepository.saveAll(areaMuServicesAdded);
        areaMuServicesAdded.forEach(amsa -> historyHelper.sendHistory(null, amsa, AreaMuService.class));

        //7.2
        Map<AreaMuService, AreaMuService> deletedAreaMuServicesAdded = new HashMap<>();
        for (Long servicedMuId : closeServicedMuIds) {
            if (servicedMuId != null) {
                List<AreaMuService> amsActive = areaMuServiceRepository.findActive(servicedMuId, areaId);

                LocalDate now = LocalDate.now();
                amsActive.forEach(ams -> {
                    AreaMuService amsOld = historyHelper.clone(ams);
                    ams.setEndDate(now.minusDays(1));
                    deletedAreaMuServicesAdded.put(amsOld, areaMuServiceRepository.save(ams));
                });

            }
        }
        // Логирование изменений
        deletedAreaMuServicesAdded.forEach((o, n) -> historyHelper.sendHistory(o, n, AreaMuService.class));
    }

    @Override
    public Page<MoAddressAllocation> getMoAddressTotal(List<Long> addressGlobalIds, PageRequest paging) throws ContingentException {
        Validation validation = new Validation();
        //2.
        areaHelper.checkMaxPage(paging, validation);
        //3.
        areaHelper.checkMaxRequestAddresses(addressGlobalIds.size(), validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        Page<MoAddress> moAddresses = paging != null ? moAddressRepository.getActiveMoAddressesByGlobalIds(addressGlobalIds, paging)
                : moAddressRepository.getActiveMoAddressesByGlobalIds(addressGlobalIds);
        List<MoAddressAllocation> addresses = moAddresses.stream()
                .filter(a -> a.getAddress() != null)
                .map(a -> new MoAddressAllocation(a.getAddress().getGlobalId(), a.getMoId(), a.getAreaType(), a.getId()))
                .collect(Collectors.toList());

        return paging == null ? new PageImpl<>(addresses) : new PageImplCustom<>(addresses, paging, moAddresses.getTotalElements());
    }

    @Override
    public AreaHistory getAreaHistory(long areaId, PageRequest paging) throws ContingentException {
        Validation validation = new Validation();
        //2.
        areaHelper.checkMaxPage(paging, validation);

        // 3.
        Area area = areaHelper.checkAndGetArea(areaId, validation, false);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        // 4.
        Page<AreaHistory.Event> areaHistoryEvents = areaMedicalEmployeeRepository.areaHistory(areaId, paging);

        AreaHistory areaHistory = new AreaHistory();

        areaHistory.setAreaId(area.getId());
        areaHistory.setDateCreated(area.getCreateDate());
        areaHistory.setEvents(areaHistoryEvents);

        return areaHistory;
    }

    @Override
    public Page<AreaOrEmployeeEvent> getAreaHistory3(long areaId, PageRequest paging) throws ContingentException {
        Validation validation = new Validation();
        //2.
        areaHelper.checkMaxPage(paging, validation);
        areaHelper.checkPaging(paging, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        //3.
        Area area = areaHelper.checkAndGetArea(areaId, validation, false);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        //4.
        return historyEventRepository.findAreaAndEmployeeEvents(areaId, paging);
    }
}
