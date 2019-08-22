package moscow.ptnl.contingent.area.service;

import moscow.ptnl.contingent.area.entity.area.AddressAllocationOrders;
import moscow.ptnl.contingent.area.entity.area.Addresses;
import moscow.ptnl.contingent.area.entity.area.Area;
import moscow.ptnl.contingent.area.entity.area.AreaAddress;
import moscow.ptnl.contingent.area.entity.area.AreaMedicalEmployees;
import moscow.ptnl.contingent.area.entity.area.AreaToAreaType;
import moscow.ptnl.contingent.area.entity.area.MoAddress;
import moscow.ptnl.contingent.area.entity.area.MoAvailableAreaTypes;
import moscow.ptnl.contingent.area.entity.area.MuAvailableAreaTypes;
import moscow.ptnl.contingent.area.entity.nsi.NsiAddressFormingElement;
import moscow.ptnl.contingent.area.entity.nsi.AreaPolicyTypes;
import moscow.ptnl.contingent.area.entity.nsi.AreaType;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeKindEnum;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeMedicalPositions;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeSpecializations;
import moscow.ptnl.contingent.area.entity.nsi.NsiBuildingRegistry;
import moscow.ptnl.contingent.area.entity.nsi.PolicyType;
import moscow.ptnl.contingent.area.entity.nsi.PositionCode;
import moscow.ptnl.contingent.area.entity.nsi.PositionNom;
import moscow.ptnl.contingent.area.entity.nsi.Specialization;
import moscow.ptnl.contingent.area.error.AreaErrorReason;
import moscow.ptnl.contingent.area.error.ContingentException;
import moscow.ptnl.contingent.area.error.Validation;
import moscow.ptnl.contingent.area.error.ValidationParameter;
import moscow.ptnl.contingent.area.model.area.AddressLevelType;
import moscow.ptnl.contingent.area.model.area.AddressWrapper;
import moscow.ptnl.contingent.area.model.area.AreaInfo;
import moscow.ptnl.contingent.area.model.area.AreaTypeStateType;
import moscow.ptnl.contingent.area.model.area.MuAreaTypesFull;
import moscow.ptnl.contingent.area.model.area.NsiAddress;
import moscow.ptnl.contingent.repository.area.AddressAllocationOrderCRUDRepository;
import moscow.ptnl.contingent.repository.area.AddressAllocationOrderRepository;
import moscow.ptnl.contingent.repository.area.AddressesCRUDRepository;
import moscow.ptnl.contingent.repository.area.AddressesRepository;
import moscow.ptnl.contingent.repository.area.AreaAddressCRUDRepository;
import moscow.ptnl.contingent.repository.area.AreaAddressRepository;
import moscow.ptnl.contingent.repository.area.AreaCRUDRepository;
import moscow.ptnl.contingent.repository.area.AreaMedicalEmployeeCRUDRepository;
import moscow.ptnl.contingent.repository.area.AreaMedicalEmployeeRepository;
import moscow.ptnl.contingent.repository.area.AreaRepository;
import moscow.ptnl.contingent.repository.area.AreaToAreaTypeCRUDRepository;
import moscow.ptnl.contingent.repository.area.AreaToAreaTypeRepository;
import moscow.ptnl.contingent.repository.area.MoAddressCRUDRepository;
import moscow.ptnl.contingent.repository.area.MoAddressRepository;
import moscow.ptnl.contingent.repository.area.MoAvailableAreaTypesCRUDRepository;
import moscow.ptnl.contingent.repository.area.MoAvailableAreaTypesRepository;
import moscow.ptnl.contingent.repository.area.MuAvailableAreaTypesCRUDRepository;
import moscow.ptnl.contingent.repository.area.MuAvailableAreaTypesRepository;
import moscow.ptnl.contingent.repository.nsi.AddressFormingElementRepository;
import moscow.ptnl.contingent.repository.nsi.AreaPolicyTypesCRUDRepository;
import moscow.ptnl.contingent.repository.nsi.AreaPolicyTypesRepository;
import moscow.ptnl.contingent.repository.nsi.AreaTypeMedicalPositionsRepository;
import moscow.ptnl.contingent.repository.nsi.AreaTypeSpecializationsRepository;
import moscow.ptnl.contingent.repository.nsi.AreaTypesCRUDRepository;
import moscow.ptnl.contingent.repository.nsi.BuildingRegistryRepository;
import moscow.ptnl.contingent.repository.nsi.PolicyTypeRepository;
import moscow.ptnl.contingent.repository.nsi.PositionCodeRepository;
import moscow.ptnl.contingent.repository.nsi.PositionNomRepository;
import moscow.ptnl.contingent.area.util.Period;
import moscow.ptnl.contingent.service.history.HistoryService;
import moscow.ptnl.util.Strings;
import moscow.ptnl.ws.security.UserContextHolder;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import ru.mos.emias.contingent2.core.AddMedicalEmployee;
import ru.mos.emias.contingent2.core.ChangeMedicalEmployee;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.TreeMap;
import java.util.stream.Collectors;

import moscow.ptnl.contingent.domain.esu.event.annotation.LogESU;

import moscow.ptnl.contingent.domain.esu.event.AreaInfoEvent;

@Service
public class AreaServiceInternalImpl implements AreaServiceInternal {

    private final static Logger LOG = LoggerFactory.getLogger(AreaServiceInternalImpl.class);

    @Autowired
    private AreaTypesCRUDRepository areaTypesCRUDRepository;

    @Autowired
    private AreaRepository areaRepository;

    @Autowired
    private AreaCRUDRepository areaCRUDRepository;

    @Autowired
    private AreaToAreaTypeCRUDRepository areaToAreaTypeCRUDRepository;

    @Autowired
    private AreaToAreaTypeRepository areaToAreaTypeRepository;

    @Autowired
    private AddressAllocationOrderCRUDRepository addressAllocationOrderCRUDRepository;

    @Autowired
    private AddressAllocationOrderRepository addressAllocationOrderRepository;

    @Autowired
    private AreaMedicalEmployeeCRUDRepository areaMedicalEmployeeCRUDRepository;

    @Autowired
    private AreaTypeMedicalPositionsRepository areaTypeMedicalPositionsRepository;

    @Autowired
    private AreaMedicalEmployeeRepository areaMedicalEmployeeRepository;

    @Autowired
    private BuildingRegistryRepository buildingRegistryRepository;

    @Autowired
    private AddressesCRUDRepository addressesCRUDRepository;

    @Autowired
    private AddressesRepository addressesRepository;

    @Autowired
    private MoAddressCRUDRepository moAddressCRUDRepository;

    @Autowired
    private MoAddressRepository moAddressRepository;

    @Autowired
    private AreaAddressRepository areaAddressRepository;

    @Autowired
    private AreaAddressCRUDRepository areaAddressCRUDRepository;

    @Autowired
    private MoAvailableAreaTypesCRUDRepository moAvailableAreaTypesCRUDRepository;

    @Autowired
    private MoAvailableAreaTypesRepository moAvailableAreaTypesRepository;

    @Autowired
    private AreaServiceHelper areaHelper;

    @Autowired
    private AreaAddressChecker areaAddressChecker;

    @Autowired
    private EsuHelperService esuHelperService;

    @Autowired
    private SettingService settingService;

    @Autowired
    private AddressFormingElementRepository addressFormingElementRepository;

    @Autowired
    private MuAvailableAreaTypesCRUDRepository muAvailableAreaTypesCRUDRepository;

    @Autowired
    private MuAvailableAreaTypesRepository muAvailableAreaTypesRepository;

    @Autowired
    private Algorithms algorithms;

    @Autowired
    private HistoryService historyService;

    @Autowired
    private PositionNomRepository positionNomRepository;

    @Autowired
    private AreaTypeSpecializationsRepository areaTypeSpecializationsRepository;

    @Autowired
    private AreaPolicyTypesCRUDRepository areaPolicyTypesCRUDRepository;

    @Autowired
    private AreaPolicyTypesRepository areaPolicyTypesRepository;

    @Autowired
    private PolicyTypeRepository policyTypeRepository;

    @Autowired
    private PositionCodeRepository positionCodeRepository;

    public AreaServiceInternalImpl() {
    }

    public AreaServiceInternalImpl(Algorithms algorithms, AreaServiceHelper areaHelper) {
        this.algorithms = algorithms;
        this.areaHelper = areaHelper;
    }

    // (К_УУ_1) Добавление типов участков, доступных для МО
    @Override
    public void addMoAvailableAreaTypes(long moId, List<Long> areaTypeCodes) throws ContingentException {
        areaTypeCodes = areaTypeCodes.stream().distinct().collect(Collectors.toList());
        Validation validation = new Validation();
        List<AreaType> areaTypes = areaHelper.checkAndGetAreaTypesExist(areaTypeCodes, validation);
        areaHelper.checkAreaTypesExistInMO(moId, areaTypes, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        // TODO перееписать с saveAll
        areaTypeCodes.forEach(a -> {
            MoAvailableAreaTypes availableAreaType = new MoAvailableAreaTypes();
            areaTypes.stream()
                    .filter(t -> Objects.equals(t.getCode(), a))
                    .findFirst()
                    .ifPresent(availableAreaType::setAreaType);
            availableAreaType.setMoId(moId);
            availableAreaType.setCreateDate(LocalDateTime.now());
            moAvailableAreaTypesCRUDRepository.save(availableAreaType);
        });
    }

    // (К_УУ_2)	Удаление типов участков из доступных для МО
    @Override
    public void delMoAvailableAreaTypes(long moId, List<Long> areaTypeCodes) throws ContingentException {
        areaTypeCodes = areaTypeCodes.stream().distinct().collect(Collectors.toList());
        Validation validation = new Validation();
        List<MoAvailableAreaTypes> moAvailableAreaTypes = areaHelper.checkAndGetAreaTypesNotExistInMO(moId, areaTypeCodes, validation);
        areaHelper.checkAndGetAreaTypesNotExistInMU(moAvailableAreaTypes, areaTypeCodes, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        // TODO перееписать с deleteAll
        moAvailableAreaTypes.forEach(a -> moAvailableAreaTypesCRUDRepository.delete(a));
    }

    // (К_УУ_3)	Предоставление типов участков, доступных для МО
    @Override
    public List<AreaType> getMoAvailableAreaTypes(long moId) throws ContingentException {
        List<MoAvailableAreaTypes> moAvailableAreaTypes = moAvailableAreaTypesRepository.findAreaTypes(moId);

        return moAvailableAreaTypes.stream().map(MoAvailableAreaTypes::getAreaType).collect(Collectors.toList());
    }

    // (К_УУ_4)	Добавление типов, доступных для МУ
    @Override
    public void addMuAvailableAreaTypes(long moId, long muId, List<Long> areaTypeCodes) throws ContingentException {
        areaTypeCodes = areaTypeCodes.stream().distinct().collect(Collectors.toList());
        Validation validation = new Validation();
        // 1.
        List<MoAvailableAreaTypes> moAvailableAreaTypes = areaHelper.checkAndGetAreaTypesNotExistInMO(moId, areaTypeCodes, validation);
        // 2.
        areaHelper.checkAreaTypesExistInMU(muId, moAvailableAreaTypes.stream()
                .map(MoAvailableAreaTypes::getAreaType)
                .collect(Collectors.toList()), validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        // 3.
        moAvailableAreaTypes.forEach(a -> {
            MuAvailableAreaTypes muAvailableAreaType = new MuAvailableAreaTypes();
            muAvailableAreaType.setMuId(muId);
            muAvailableAreaType.setAreaType(a.getAreaType());
            muAvailableAreaType.setMoAvailableAreaType(a);
            muAvailableAreaType.setCreateDate(LocalDateTime.now());
            muAvailableAreaTypesCRUDRepository.save(muAvailableAreaType);
        });
    }

    // (К_УУ_5)	Удаление типов участков из доступных для МУ
    @Override
    public void delMuAvailableAreaTypes(long muId, List<Long> areaTypeCodes) throws ContingentException {
        areaTypeCodes = areaTypeCodes.stream().distinct().collect(Collectors.toList());
        Validation validation = new Validation();
        // 1.
        List<MuAvailableAreaTypes> areaTypes = areaHelper.checkAndGetAreaTypesNotExistInMU(muId, areaTypeCodes, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        // 2.
        muAvailableAreaTypesCRUDRepository.deleteAll(areaTypes);
    }

    // (К_УУ_6)	Предоставление типов участков, доступных для МУ
    @Override
    public MuAreaTypesFull getMuAvailableAreaTypes(long moId, long muId, AreaTypeStateType areaTypeState) throws ContingentException {
        // 1.
        List<AreaType> usedAreaTypes = muAvailableAreaTypesRepository.findAreaTypes(muId).stream()
                .map(MuAvailableAreaTypes::getAreaType)
                .collect(Collectors.toList());
        // 2.
        List<AreaType> availableAreaTypes = new ArrayList<>();

        if (!AreaTypeStateType.USED_IN_MU.equals(areaTypeState)) {
            availableAreaTypes.addAll(moAvailableAreaTypesRepository.findAreaTypes(moId).stream()
                    .filter(a -> usedAreaTypes.stream()
                                    .noneMatch(b -> Objects.equals(b, a.getAreaType())))
                    .map(MoAvailableAreaTypes::getAreaType)
                    .collect(Collectors.toList()));
        }
        return new MuAreaTypesFull(AreaTypeStateType.AVAILABLE_TO_ADD.equals(areaTypeState) ? new ArrayList<>() : usedAreaTypes,
                availableAreaTypes);
    }

    @Override
    public void archiveAreaComposit(long areaId, long muId) throws ContingentException {
    }

    // (К_УУ_7)	Создание участка обслуживания первичного типа
    @Override @LogESU(type = AreaInfoEvent.class, useResult = true)
    public Long createPrimaryArea(long moId, Long muId, Integer number, Long areaTypeCode, List<Long> policyTypesIds,
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
        Area area = new Area(moId, muId, areaType, number, autoAssignForAttachment, false, description,
                attachByMedicalReason, ageMin, ageMax, ageMinM, ageMaxM, ageMinW, ageMaxW, LocalDateTime.now());
        areaCRUDRepository.save(area);

        // 13
        List<PolicyType> policyTypes = policyTypeRepository.findByIds(policyTypesIds);
        List<AreaPolicyTypes> areaPolicyTypes = policyTypes.stream().map(policyType ->
                new AreaPolicyTypes(area, policyType)).collect(Collectors.toList());
        areaPolicyTypesCRUDRepository.saveAll(areaPolicyTypes);

        // 15
        return area.getId();
    }

    // (К_УУ_8)	Создание участка обслуживания зависимого типа
    @Override
    public Long createDependentArea(long moId, Long muId, Integer number, Long areaTypeCode,
                                    List<Long> primaryAreaTypeCodesIds, List<Long> policyTypeCodesIds,
                                    Integer ageMin, Integer ageMax, Integer ageMinM, Integer ageMaxM, Integer ageMinW, Integer ageMaxW,
                                    String description) throws ContingentException {

        Validation validation = new Validation();

        // 1.
        List<AreaType> areaTypes = areaHelper.checkAndGetAreaTypesExist(Collections.singletonList(areaTypeCode), validation);
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        AreaType areaType = areaTypes.get(0);

        areaHelper.checkAreaTypeIsDependent(areaType, validation);
        // 2.
        primaryAreaTypeCodesIds = primaryAreaTypeCodesIds.stream().distinct().collect(Collectors.toList());

        // 3.
        primaryAreaTypeCodesIds.forEach(code -> {
            // TODO позже добавить проверку что primaryAreaTypeCode есть в таблице areaTypes
            AreaType primaryAreaType = areaTypesCRUDRepository.findById(code).get();
            areaHelper.checkAreaTypeIsPrimary(primaryAreaType, validation);
            areaHelper.checkPrimaryAreasTypesInMUProfile(moId, muId, primaryAreaType, validation);
        });
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        // 4.
        if (!areaRepository.findAreas(moId, muId, areaTypeCode, null, true).isEmpty()) {
            validation.error(AreaErrorReason.AREA_WITH_TYPE_EXISTS_IN_MO, new ValidationParameter("areaTypeCode", areaTypeCode));
        }
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        // 5. TODO очень странно..
        areaHelper.checkPolicyTypesDepArea(policyTypeCodesIds, validation);
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        // 6.
        areaHelper.checkAreaTypeAgeSetups(areaType, ageMin, ageMax, ageMinM, ageMaxM, ageMinW, ageMaxW, validation);
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        // 7.
        Area area = new Area(moId, muId, areaType, number, null, false, description,
                null, ageMin, ageMax, ageMinM, ageMaxM, ageMinW, ageMaxW, LocalDateTime.now());
        areaCRUDRepository.save(area);

        // 8.
        for (Long areaTypeCodeId : primaryAreaTypeCodesIds) {
            AreaToAreaType areaToAreaType = new AreaToAreaType();
            areaToAreaType.setArea(area);
            areaToAreaType.setAreaType(areaTypesCRUDRepository.findById(areaTypeCodeId).get());
            areaToAreaTypeCRUDRepository.save(areaToAreaType);
        }
        // 9.
        List<PolicyType> policyTypeList = policyTypeRepository.findByIds(policyTypeCodesIds);

        if (policyTypeCodesIds != null && !policyTypeCodesIds.isEmpty()) {
            AreaPolicyTypes areaPolicyTypes = new AreaPolicyTypes();
            areaPolicyTypes.setArea(area);
            areaPolicyTypes.setPolicyType(policyTypeList.get(0));
            areaPolicyTypesCRUDRepository.save(areaPolicyTypes);
        }

        // 10.
        List<Area> primAreas = areaRepository.findAreas(moId, muId, primaryAreaTypeCodesIds, null, true);

        // 11.
        esuHelperService.sendAttachOnAreaChangeEvent(primAreas.stream().map(Area::getId).collect(Collectors.toList()),
                null, area);

        // 12.
        return area.getId();
    }

    // (К_УУ_9)	Изменение участка обслуживания первичного типа
    @Override @LogESU(type = AreaInfoEvent.class, parameters = {"areaId"})
    public void updatePrimaryArea(long areaId, Integer number, List<Long> policyTypesAddIds, List<Long> policyTypesDelIds,
                                  Integer ageMin, Integer ageMax, Integer ageMinM, Integer ageMaxM, Integer ageMinW, Integer ageMaxW,
                                  Boolean autoAssignForAttachment, Boolean attachByMedicalReason, String description) throws ContingentException {

        Validation validation = new Validation();

        // 1, 2
        Area area = areaHelper.checkAndGetArea(areaId, validation);
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        Area oldArea = historyService.clone(area);

        // 3
        if (number != null) {
            areaHelper.checkAreaExistsInMU(area.getMuId(), area.getMoId(), area.getAreaType(), number, area.getId(), validation);
        }

        // 4
        areaHelper.checkPolicyTypesIsOMS(policyTypesAddIds, validation);

        // 5
        List<PolicyType> policyTypesDel = policyTypeRepository.findByIds(policyTypesDelIds);
        areaHelper.checkPolicyTypesDel(area, policyTypesDel, validation);

        // 6
        if (Boolean.TRUE.equals(autoAssignForAttachment)) {
            if (area.getAreaType().getMpguAvailable() != null
                    && !Boolean.TRUE.equals(area.getAreaType().getMpguAvailable())) {
                validation.error(AreaErrorReason.CANT_SET_AUTO_ASSIGN_FOR_ATTACHMENT,
                        new ValidationParameter("areaType", area.getAreaType().getTitle()));
            }
           if (Boolean.TRUE.equals(attachByMedicalReason)) {
                validation.error(AreaErrorReason.AREA_FLAGS_INCORRECT);
            }
        }

        // 7
        if (attachByMedicalReason != null && area.getAreaType().getAttachByMedicalReason() != null &&
                !Objects.equals(attachByMedicalReason, area.getAreaType().getAttachByMedicalReason())) {
            validation.error(AreaErrorReason.ATTACH_BY_MEDICAL_REASON_INCORRECT,
                    new ValidationParameter("attachByMedicalReason", attachByMedicalReason),
                    new ValidationParameter("attachByMedicalReason", area.getAreaType().getAttachByMedicalReason()));
        }

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
        areaCRUDRepository.save(area);

        // 10
        List<PolicyType> policyTypesAdd = policyTypeRepository.findByIds(policyTypesAddIds);
        areaHelper.saveAndDeleteAreaPolicyTypes(area, policyTypesAdd, policyTypesDel);

        // 11
        //if (areaHelper.isAreaPrimary(area)) {
        //    esuHelperService.sendAreaInfoEvent(area, "updatePrimaryArea");
        //}

        // Логирование изменений
        historyService.write(UserContextHolder.getPrincipal(), oldArea, area);
    }

    // (К_УУ_10) Изменение участка обслуживания зависимого типа
    @Override
    public void updateDependentArea(long areaId, Long muId, Integer number, String description,
                                    List<Long> primaryAreaTypeCodesAddIds, List<Long> primaryAreaTypeCodesDelIds,
                                    List<Long> policyTypesAddIds, List<Long> policyTypesDelIds,
                                    Integer ageMin, Integer ageMax, Integer ageMinM, Integer ageMaxM, Integer ageMinW, Integer ageMaxW) throws ContingentException {

        Validation validation = new Validation();
        //1. и 2.
        Area area = areaHelper.checkAndGetArea(areaId, validation);


        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        Area oldArea = historyService.clone(area);

        // 3.
        if (muId != null && (area.getAreaType().getAreaTypeKind() == null ||
                !Objects.equals(area.getAreaType().getAreaTypeKind().getCode(), AreaTypeKindEnum.PERSONAL.getCode()))) {
            validation.error(AreaErrorReason.AREA_NOT_RELATED_TO_SPECIAL_OFFICE);
        }
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        // 4.
        primaryAreaTypeCodesAddIds = primaryAreaTypeCodesAddIds.stream().distinct().collect(Collectors.toList());
        primaryAreaTypeCodesDelIds = primaryAreaTypeCodesDelIds.stream().distinct().collect(Collectors.toList());

        primaryAreaTypeCodesAddIds.forEach(pat -> {
            // TODO позже добавить проверку что primaryAreaTypeCode есть в таблице areaTypes
            AreaType areaType = areaTypesCRUDRepository.findById(pat).get();
            // 5.1
            areaHelper.checkAreaDependsOnPrimaryAreaType(area, areaType, validation);
            // 5.2
            areaHelper.checkAreaTypeIsPrimary(areaType, validation);
            // 5.3
            areaHelper.checkPrimaryAreasTypesInMUProfile(area.getMoId(), muId, areaType, validation);
            }
        );

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        // 6.
        List<AreaToAreaType> areaTypeToDeleteBd = null;
        if (!primaryAreaTypeCodesDelIds.isEmpty()) {
            areaTypeToDeleteBd = areaToAreaTypeRepository.findAreaTypesByAreaAndTypeCode(area, primaryAreaTypeCodesDelIds);
            for (Long areaTypeToDelete : primaryAreaTypeCodesDelIds) {
                if (areaTypeToDeleteBd.stream().noneMatch(areaType -> areaType.getAreaType().getCode().equals(areaTypeToDelete))) {
                    validation.error(AreaErrorReason.AREA_NOT_DEPEND_ON_AREA_TYPE,
                            new ValidationParameter("areaId", areaId),
                            new ValidationParameter("areTypeTitle", areaTypesCRUDRepository.findById(areaTypeToDelete).get().getTitle()));
                }
            }
            if (!validation.isSuccess()) {
                throw new ContingentException(validation);
            }
        }

        // 7.
        areaHelper.checkPolicyTypesDepArea(policyTypesAddIds, validation);
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        List<PolicyType> policyTypesAdd = policyTypeRepository.findByIds(policyTypesAddIds);

        // 8.
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

        // 9.
        areaHelper.checkAreaTypeAgeSetups(area.getAreaType(), ageMin, ageMax, ageMinM, ageMaxM, ageMinW, ageMaxW, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        // 10.
        Long muIdFinal = muId == null ? area.getMuId() : muId;
        area.setMuId(muIdFinal);
        area.setNumber(number == null ? area.getNumber() : number);
        area.setAgeMax(ageMax == null ? area.getAgeMax() : ageMax);
        area.setAgeMin(ageMin == null ? area.getAgeMin() : ageMin);
        area.setAgeMMax(ageMaxM == null ? area.getAgeMMax() : ageMaxM);
        area.setAgeMMin(ageMinM == null ? area.getAgeMMin() : ageMinM);
        area.setAgeWMax(ageMaxW == null ? area.getAgeWMax() : ageMaxW);
        area.setAgeWMin(ageMinW == null ? area.getAgeWMin() : ageMinW);
//        area.setAttachByMedicalReason(); TODO нет такого входного параметра
        area.setDescription(description == null ? area.getDescription() : description);
        area.setUpdateDate(LocalDateTime.now());
        areaCRUDRepository.save(area);

        // 11.
        // 11.1.
        primaryAreaTypeCodesAddIds.forEach(pata -> {
            AreaType areaType = areaTypesCRUDRepository.findById(pata).get();
            AreaToAreaType areaToAreaType = new AreaToAreaType();
            areaToAreaType.setArea(area);
            areaToAreaType.setAreaType(areaType);
            areaToAreaTypeCRUDRepository.save(areaToAreaType);
        });
        // 11.2.
        if (!primaryAreaTypeCodesDelIds.isEmpty()) {
            areaToAreaTypeCRUDRepository.deleteAll(areaTypeToDeleteBd);
        }

        // 12.
        areaHelper.saveAndDeleteAreaPolicyTypes(area, policyTypesAdd, policyTypesDel);

        // Вид участка не именной
        if (area.getAreaType() != null && area.getAreaType().getAreaTypeKind() != null &&
                !area.getAreaType().getAreaTypeKind().getCode().equals(AreaTypeKindEnum.PERSONAL.getCode())) {
            // 13.
            if (!primaryAreaTypeCodesAddIds.isEmpty()) {
                // 13.1.
                List<Area> primAreasAdd = areaRepository.findAreas(area.getMoId(), area.getMuId(), primaryAreaTypeCodesAddIds, null, true);

                // 13.2.
                if (primAreasAdd != null && !primAreasAdd.isEmpty()) {
                    esuHelperService.sendAttachOnAreaChangeEvent(
                            primAreasAdd.stream().map(Area::getId).collect(Collectors.toList()),
                            null,
                            area);
                }
            }

            // 14.
            if (!primaryAreaTypeCodesDelIds.isEmpty()) {
                // 14.1.
                List<Area> primAreasDel = areaRepository.findAreas(area.getMoId(), area.getMuId(), primaryAreaTypeCodesDelIds, null, true);
                // 14.2.
                if (primAreasDel != null && !primAreasDel.isEmpty()) {
                    esuHelperService.sendAttachOnAreaChangeEvent(
                            null,
                            primAreasDel.stream().map(Area::getId).collect(Collectors.toList()),
                            area);
                }
            }
        }

        // Логирование изменений
        historyService.write(UserContextHolder.getPrincipal(), oldArea, area);

        // 15.
        return;
    }

    // (К_УУ_11) Изменение медицинских работников на участке обслуживания
    @Override @LogESU(type = AreaInfoEvent.class, parameters = {"areaId"})
    public List<Long> setMedicalEmployeeOnArea(long areaId, List<AddMedicalEmployee> addEmployeesInput,
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
        areaMedicalEmployeeCRUDRepository.findAllById(changeIds).forEach(changeEmployeesDb::add);

        if (!AreaTypeKindEnum.MILDLY_ASSOCIATED.equalsCode(area.getAreaType().getAreaTypeKind().getCode()) 
                && !AreaTypeKindEnum.TREATMENT_ROOM_ASSOCIATED.equalsCode(area.getAreaType().getAreaTypeKind().getCode())) 
        {

            addEmployeesInput.stream().filter(empl -> !empl.isIsReplacement())
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
                        new ValidationParameter("id", inputEmpl.getAssignmentId()));
            } else {
                emplDb = employee.get();
                //4.1
                if (inputEmpl.getEndDate() != null && inputEmpl.getEndDate().isBefore(LocalDate.now())
                        || employee.get().getArea().getId() != areaId) {
                    validation.error(AreaErrorReason.EMPLOYEE_NOT_RELATED_TO_AREA,
                            new ValidationParameter("id", inputEmpl.getAssignmentId()));
                }
            }

            //4.2
            if (inputEmpl.getEndDate() == null && inputEmpl.getStartDate() == null) {
                validation.error(AreaErrorReason.NOTHING_TO_CHANGE);
            }

            //4.3
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
            Optional<PositionCode> positionCodeOptional = positionCodeRepository.getByCode(empl.getPositionCode());
            if (!positionCodeOptional.isPresent()) {
                continue;
            }

            // 5.3.2.
            PositionCode positionCode = positionCodeOptional.get();
            Optional<PositionNom> positionNom = positionNomRepository.getByPositionCodeId(positionCode.getGlobalId());

            if (!positionNom.isPresent()) {
                // TODO ???
                continue;
            }

            //5.4.
            Specialization specialization = positionNom.get().getSpecialization();

            // 5.5.
            if (specialization != null && areaTypeSpecializations.stream().noneMatch(ats -> ats.getSpecializationCode().equals(specialization.getCode()))) {
                validation.error(AreaErrorReason.SPECIALIZATION_NOT_RELATED_TO_AREA,
                        new ValidationParameter("InputSpecialization", specialization.getTitle()),
                        new ValidationParameter("jobInfoId", empl.getMedicalEmployeeJobId()),
                        new ValidationParameter("AreaSpecialization", area.getAreaType().getCode()));

            }
            
            // 5.6.            
            List<AreaTypeMedicalPositions> positions = areaTypeMedicalPositionsRepository.getPositionsByAreaType(area.getAreaType().getCode());

            if (positions != null && positions.stream().noneMatch(pos -> pos.getPositionCode().getCode().equals(empl.getPositionCode()))) {
                validation.error(AreaErrorReason.POSITION_NOT_SET_FOR_AREA_TYPE,
                        new ValidationParameter("positionTitle", positionNom.get().getTitle()),
                        new ValidationParameter("jobInfoId", empl.getMedicalEmployeeJobId()),
                        new ValidationParameter("areaTypeName", area.getAreaType().getTitle()));
            }
        }
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        //6.1
        List<AreaMedicalEmployees> allEmployees = new ArrayList<>(areaEmployeesDb);
        areaHelper.applyChanges(allEmployees, changeEmployeesInput);
        areaHelper.addNew(allEmployees, addEmployeesInput, area);
        areaHelper.checkDatesNotInterceptWithSamePosition(allEmployees, validation);

        //6.2
        List<AreaMedicalEmployees> mainEmployees =
                areaEmployeesDb.stream().filter(empl -> !empl.getReplacement()).collect(Collectors.toList());
        areaHelper.applyChanges(mainEmployees, changeEmployeesInput);
        areaHelper.addNew(mainEmployees, addEmployeesInput.stream()
                .filter(empl -> !empl.isIsReplacement()).collect(Collectors.toList()), area);
        if (area.getAreaType() != null && area.getAreaType().getAreaTypeKind() != null &&
                area.getAreaType().getAreaTypeKind().getCode() == AreaTypeKindEnum.MILDLY_ASSOCIATED.getCode()) {
            areaHelper.checkMainEmployeesOverlappingDates(mainEmployees, validation);
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
                        .filter(AddMedicalEmployee::isIsReplacement).collect(Collectors.toList()), area);
                replacementEmployees.sort(Comparator.comparing(AreaMedicalEmployees::getStartDate));
                areaHelper.checkReplacementWithoutMain(periodsWithoutMainEmpl, replacementEmployees, validation);
            }
        }

        //7
        List<Long> result = new ArrayList<>();
        areaHelper.applyChanges(changeEmployeesDb, changeEmployeesInput);
        areaHelper.addNew(changeEmployeesDb, addEmployeesInput, area);
        areaMedicalEmployeeCRUDRepository.saveAll(changeEmployeesDb).forEach(saved -> {
            if (!areaEmployeesDb.contains(saved)) {
                result.add(saved.getId());
            }
        });

        // 8
        //if (areaHelper.isAreaPrimary(area)) {
        //    esuHelperService.sendAreaInfoEvent(area, "setMedicalEmployeeOnArea");
        //}

        return result;
    }

    // (К_УУ_12) Получение подробной информации об участке
    @Override
    public AreaInfo getAreaById(Long id) throws ContingentException {

        // 1.
        Optional<Area> areaOptional = areaCRUDRepository.findById(id);
        if (!areaOptional.isPresent()) {
            throw new ContingentException(new Validation().error(AreaErrorReason.AREA_NOT_FOUND, new ValidationParameter("areaId", id)));
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

    // (К_УУ_13) Добавление адресов на участок обслуживания
    @Override @LogESU(type = AreaInfoEvent.class, parameters = {"areaId"})
    public List<Long> addAreaAddress(Long areaId, List<NsiAddress> nsiAddresses) throws ContingentException {
        Validation validation = new Validation();

        // 1. и 2.
        Area area = areaHelper.checkAndGetArea(areaId, validation);
        if (!validation.isSuccess()) { throw new ContingentException(validation); }

        // 3.
        areaHelper.checkTooManyAddresses(nsiAddresses, settingService.getPar1());

        // 4.
        areaAddressChecker.checkNsiAddresses(nsiAddresses, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        // Список все адресов с привязанными мета-данными
        List<AddressWrapper> addressWrapperList = areaHelper.convertToAddressWrapper(nsiAddresses);

        // 5. Обогащение обертки объектами территорией обслуживания МО
        areaHelper.addMoAddressToAddressWrapper(area, addressWrapperList, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        // 6.
        areaHelper.checkAddressNotServiceByAreaType(area, addressWrapperList, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        // 7.
        addressWrapperList.forEach(addressWrapper -> {
            if (addressWrapper.getNsiAddress() != null) {
                NsiAddress nsiAddress = addressWrapper.getNsiAddress();
                NsiAddressFormingElement addressFormingElement = null;
                NsiBuildingRegistry buildingRegistry = null;

                if (nsiAddress.getLevelAddress() != 8) {
                    addressFormingElement = addressFormingElementRepository.getAddressFormingElements(nsiAddress.getGlobalId(), nsiAddress.getLevelAddress()).get(0);
                } else {
                    buildingRegistry = buildingRegistryRepository.getBuildingsRegistry(nsiAddress.getGlobalId()).get(0);
                }

                List<Addresses> foundAddresses = addressesRepository.findAddresses(nsiAddress.getLevelAddress(), buildingRegistry, addressFormingElement);

                if (!foundAddresses.isEmpty()) {
                    addressWrapper.setAddress(foundAddresses.get(0));
                } else {
                    Addresses address = new Addresses();
                    //TODO fix
//                    address.setLevel(nsiAddress.getLevelAddress());
//                    address.setAddressFormingElement(addressFormingElement);
//                    address.setBuildingRegistry(buildingRegistry);
                    addressWrapper.setAddress(addressesCRUDRepository.save(address));
                }

            }
        });

        // 8.
        List<Long> areaAddressIds = new ArrayList<>();
        addressWrapperList.forEach(addressWrapper -> {
            AreaAddress areaAddress = new AreaAddress();
            areaAddress.setArea(area);
            areaAddress.setMoAddress(areaAddress.getMoAddress());
            areaAddress.setAddress(addressWrapper.getAddress());
            areaAddress.setCreateDate(LocalDateTime.now());
            areaAddress.setUpdateDate(LocalDateTime.now());
            areaAddressIds.add(areaAddressCRUDRepository.save(areaAddress).getId());
        });

        // 9. аннотация @LogESU

        return areaAddressIds;
    }

    // (К_УУ_14) Удаление адресов из участка обслуживания
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
                validation.error(AreaErrorReason.MO_ADDRESS_NOT_EXISTS, new ValidationParameter("areaAddressId", aai));
            }
        });
        if (!validation.isSuccess()) { throw new ContingentException(validation); }

        // 5.
        LocalDate localDate = LocalDate.now();
        for (AreaAddress areaAddress: areaAddresses) {
            if (!areaAddress.getStartDate().equals(localDate)) {
                // 5.1.
                areaAddress.setEndDate(localDate.minusDays(1L));
                areaAddressCRUDRepository.save(areaAddress);
            } else {
                // 5.2.
                areaAddressCRUDRepository.delete(areaAddress);
            }
        }

        // 6.
        //if (areaHelper.isAreaPrimary(area)) {
        //    esuHelperService.sendAreaInfoEvent(area, "delAreaAddress");
        //}

        // 7.
        return;
    }

    // (К_УУ_15) Получение списка адресов участка обслуживания
    @Override
    public Page<moscow.ptnl.contingent.area.model.area.AddressArea> getAreaAddress(long areaId, PageRequest paging) throws ContingentException {
        Validation validation = new Validation();

        // 1.
        areaHelper.checkMaxPage(paging, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        // 2.
        List<AreaAddress> areaAddresses = areaAddressRepository.findAreaAddressesByAreaId(areaId);

        if (areaAddresses.isEmpty()) {
            return new PageImpl<moscow.ptnl.contingent.area.model.area.AddressArea>(new ArrayList<>(), paging, 0);
        }

        // 3.
        Map<Long, Addresses>  addresses = new TreeMap<>();
        areaAddresses.forEach(aa -> {
            Addresses address = aa.getAddress();
            addresses.put(aa.getId(), address);
        });

        List<moscow.ptnl.contingent.area.model.area.AddressArea> addressAreas = new ArrayList<>();

        addresses.forEach((addressId, address) -> {
            //TODO fix
//            if (address.getLevel() == 8) {
//                NsiBuildingRegistry buildingRegistry = address.getBuildingRegistry();
//                NsiAddressFormingElement addressFormingElement = buildingRegistry.getAddressFormingElement();
//                addressAreas.add(new moscow.ptnl.contingent.area.model.area.AddressArea(addressId, buildingRegistry, addressFormingElement));
//            } else {
//                addressAreas.add(new moscow.ptnl.contingent.area.model.area.AddressArea(addressId, address.getAddressFormingElement()));
//            }
        });

        // 4.
        // Сортировка в TreeMap

        // 5.
        return new PageImpl<moscow.ptnl.contingent.area.model.area.AddressArea>(new ArrayList<>(addressAreas),
                paging, addressAreas.size());
    }

    // (К_УУ_16) Архивирование участка обслуживания
    @Override @LogESU(type = AreaInfoEvent.class, parameters = {"areaId"})
    public void archiveArea(long areaId) throws ContingentException {
        Validation validation = new Validation();

        // 1. 2.
        Area area = areaHelper.checkAndGetArea(areaId, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

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
        areaHelper.delAreaMedicalEmployees(new ArrayList<>(area.getActualMedicalEmployees()));

        // 6. Система для данного участка меняет статус на «Архивный»
        area.setArchived(true);
        areaCRUDRepository.save(area);

        // 7.
        //if (areaHelper.isAreaPrimary(area)) {
        //    esuHelperService.sendAreaInfoEvent(area, "archiveArea");
        //}
    }

    // (К_УУ_17) Восстановление архивного участка обслуживания
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

        // 5.
        area.setAutoAssignForAttach(false);

        // 6.
        area.setArchived(false);
        areaCRUDRepository.save(area);

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

        // 9.
        //if (areaHelper.isAreaPrimary(area)) {
        //    esuHelperService.sendAreaInfoEvent(area, "restoreArea");
        //}

        // 10.
        return;
    }

    // (К_УУ_18) Создание распоряжения
    @Override
    public Long createOrder(String number, LocalDate date, String ouz, String name) throws ContingentException {
        Validation validation = new Validation();
        areaHelper.checkDateTillToday(date, validation);

        if (!addressAllocationOrderRepository.findAddressAllocationOrders(number, date, ouz, name, false).isEmpty()) {
            validation.error(AreaErrorReason.ADDRESS_ALLOCATION_ORDER_EXISTS);
        }
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        AddressAllocationOrders order = new AddressAllocationOrders();
        order.setCreateDate(LocalDateTime.now());
        order.setUpdateDate(LocalDateTime.now());
        order.setArchived(false);
        order.setNumber(number);
        order.setDate(date);
        order.setOuz(ouz);
        order.setName(name);

        return addressAllocationOrderCRUDRepository.save(order).getId();
    }

    // (К_УУ_19) Изменение распоряжения
    @Override
    public void updateOrder(Long id, String number, LocalDate date, String ouz, String name) throws ContingentException {
        Validation validation = new Validation();

        AddressAllocationOrders order = addressAllocationOrderCRUDRepository.findById(id).orElse(null);

        if (order == null) {
            validation.error(AreaErrorReason.ADDRESS_ALLOCATION_ORDER_NOT_EXISTS,
                    new ValidationParameter("id", id));
            throw new ContingentException(validation);
        }
        if (Boolean.TRUE.equals(order.getArchived())) {
            validation.error(AreaErrorReason.ADDRESS_ALLOCATION_ORDER_IS_ARCHIVED,
                    new ValidationParameter("id", id));
        }
        if (date != null) {
            areaHelper.checkDateTillToday(date, validation);
        }

        AddressAllocationOrders oldOrder = historyService.clone(order);

        String numberNew = number == null ? order.getNumber() : number;
        LocalDate dateNew = date == null ? order.getDate() : date;
        String ouzNew = ouz == null ? order.getOuz() : ouz;
        String nameNew = name == null ? order.getName() : name;

        if (addressAllocationOrderRepository.findAddressAllocationOrders(numberNew, dateNew, ouzNew, nameNew, false).stream()
                .anyMatch(o -> !Objects.equals(o.getId(), id))) {
            validation.error(AreaErrorReason.ADDRESS_ALLOCATION_ORDER_EXISTS);
        }
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        order.setUpdateDate(LocalDateTime.now());
        order.setNumber(numberNew);
        order.setDate(dateNew);
        order.setOuz(ouzNew);
        order.setName(nameNew);

        addressAllocationOrderCRUDRepository.save(order);

        historyService.write(UserContextHolder.getPrincipal(), oldOrder, order);
    }

    // (К_УУ_20) Поиск распоряжений
    @Override
    public Page<AddressAllocationOrders> searchOrder(Long id, String number, LocalDate date, String name, PageRequest paging) throws ContingentException {
        Validation validation = new Validation();

        if (id == null && number == null && date == null && name == null) {
            validation.error(AreaErrorReason.NO_SEARCH_PARAMETERS);
            throw new ContingentException(validation);
        }
        return addressAllocationOrderRepository.findAddressAllocationOrdersOverlapped(id, number, date, name, paging);
    }

    // (К_УУ_21) Распределение жилых домов к территории обслуживания МО
    @Override
    public List<Long> addMoAddress(long moId, long areaTypeCode, long orderId, List<NsiAddress> nsiAddresses)
            throws ContingentException {
        Validation validation = new Validation();
        // 1.
        areaHelper.checkTooManyAddresses(nsiAddresses, settingService.getPar1());
        // 2.
        List<AreaType> areaTypes = areaHelper.checkAndGetAreaTypesExist(Collections.singletonList(areaTypeCode), validation);
        // 3.
        AddressAllocationOrders order = addressAllocationOrderCRUDRepository.findById(orderId).orElse(null);

        if (order == null || Boolean.TRUE.equals(order.getArchived())) {
            throw new ContingentException(AreaErrorReason.ADDRESS_ALLOCATION_ORDER_NOT_EXISTS,
                    new ValidationParameter("orderId", orderId));
        }
        // 4.
        areaAddressChecker.checkNsiAddresses(nsiAddresses, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        // 5. Система для каждого переданного адреса выполняет поиск пересекающихся распределенных адресов
        for (NsiAddress nsiAddress : nsiAddresses) {
            MoAddress moAddress = algorithms.searchServiceDistrictMOByAddress(moId, areaTypes.get(0), orderId,
                    Collections.singletonList(nsiAddress), validation);

            if (moAddress != null) {
                validation.error(AreaErrorReason.ADDRESS_ALREADY_EXISTS_1,
                        new ValidationParameter("levelAddress", nsiAddress.getLevelAddress()),
                        new ValidationParameter("globalId", nsiAddress.getGlobalId()),
                        new ValidationParameter("moId", moAddress.getMoId()),
                        new ValidationParameter("areaTypeCode", moAddress.getAreaType().getCode()));
            }
        }

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        List<AddressWrapper> addresses = areaHelper.convertAddressToWrapper(nsiAddresses);

        addresses.forEach(a -> {
            Addresses address = new Addresses();
            //TODO fix
//            if (a.nsiAddress != null) {
//                address.setLevel(a.nsiAddress.getLevelAddress());
//
//                if (!Objects.equals(a.nsiAddress.getLevelAddress(), AddressLevelType.ID.getLevel())) {
//                    address.setAddressFormingElement(a.addressFormingElement);
//                }
//                else {
//                    address.setBuildingRegistry(a.buildingRegistry);
//                }
//            }
//            // 6.
//            a.address = addressesRepository.findAddresses(address.getLevel(), address.getBuildingRegistry(), address.getAddressFormingElement())
//                    .stream().findFirst().orElseGet(() -> addressesCRUDRepository.save(address));
        });

        // 7.
        AreaType areaType = areaTypes.get(0);
        addresses.forEach(a -> {
            a.moAddress = new MoAddress();
            a.moAddress.setAddress(a.address);
            a.moAddress.setAreaType(areaType);
            a.moAddress.setMoId(moId);
            a.moAddress.setAddressAllocationOrder(order);
            a.moAddress.setStartDate(LocalDate.now());
            a.moAddress.setCreateDate(LocalDateTime.now());
            moAddressCRUDRepository.save(a.moAddress);
        });

        return addresses.stream().map(a -> a.moAddress.getId()).collect(Collectors.toList());
    }

    // (К_УУ_22) Отмена распределения жилых домов к территории обслуживания МО
    @Override
    public void delMoAddress(List<Long> moAddressIds, long orderId) throws ContingentException {
        Validation validation = new Validation();

        if (moAddressIds.size() > settingService.getPar2()) {
            validation.error(AreaErrorReason.TOO_MANY_ADDRESSES, new ValidationParameter("moAddressId", settingService.getPar2()));
        }
        List<MoAddress> addresses = areaHelper.getAndCheckMoAddressesExist(moAddressIds, validation);
        areaHelper.checkOrderExists(orderId, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        areaHelper.deleteMoAddresses(addresses);
    }

    // (К_УУ_23) Получение списка территорий обслуживания МО
    @Override
    public Page<MoAddress> getMoAddress(long moId, List<Long> areaTypeCodes, PageRequest paging) throws ContingentException {
        Validation validation = new Validation();

        // 1.
        areaHelper.checkMaxPage(paging, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        areaHelper.checkAndGetAreaTypesExist(areaTypeCodes, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        return moAddressRepository.getActiveMoAddresses(moId, areaTypeCodes, paging);
    }

    // (К_УУ_24) Получение идентификатора для создания нового участка
    @Override
    public Long getNewAreaId() throws ContingentException {
        return areaRepository.getNextAreaId();
    }

}
