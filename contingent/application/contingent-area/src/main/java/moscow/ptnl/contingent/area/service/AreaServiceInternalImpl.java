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
import moscow.ptnl.contingent.area.entity.nsi.AddressFormingElement;
import moscow.ptnl.contingent.area.entity.nsi.AreaPolicyTypes;
import moscow.ptnl.contingent.area.entity.nsi.AreaType;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeKindEnum;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeMedicalPositions;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeSpecializations;
import moscow.ptnl.contingent.area.entity.nsi.BuildingRegistry;
import moscow.ptnl.contingent.area.entity.nsi.PolicyType;
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
import moscow.ptnl.contingent.area.model.area.NotNsiAddress;
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
import moscow.ptnl.contingent.repository.area.MuAddlAreaTypesCRUDRepository;
import moscow.ptnl.contingent.repository.area.MuAddlAreaTypesRepository;
import moscow.ptnl.contingent.repository.area.MuAvailableAreaTypesCRUDRepository;
import moscow.ptnl.contingent.repository.area.MuAvailableAreaTypesRepository;
import moscow.ptnl.contingent.repository.nsi.AddressFormingElementRepository;
import moscow.ptnl.contingent.repository.nsi.AreaPolicyTypesCRUDRepository;
import moscow.ptnl.contingent.repository.nsi.AreaPolicyTypesRepository;
import moscow.ptnl.contingent.repository.nsi.AreaTypeMedicalPositionsRepository;
import moscow.ptnl.contingent.repository.nsi.AreaTypeSpecializationsRepository;
import moscow.ptnl.contingent.repository.nsi.AreaTypesCRUDRepository;
import moscow.ptnl.contingent.repository.nsi.BuildingRegistryCRUDRepository;
import moscow.ptnl.contingent.repository.nsi.BuildingRegistryRepository;
import moscow.ptnl.contingent.repository.nsi.PolicyTypeRepository;
import moscow.ptnl.contingent.repository.nsi.PositionNomCRUDRepository;
import moscow.ptnl.contingent.repository.nsi.PositionNomRepository;
import moscow.ptnl.contingent.repository.nsi.SpecializationToPositionNomRepository;
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
    private MuAddlAreaTypesRepository muAddlAreaTypesRepository;

    @Autowired
    private MuAddlAreaTypesCRUDRepository muAddlAreaTypesCRUDRepository;

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
    private SpecializationToPositionNomRepository specializationToPositionNomRepository;

    @Autowired
    private AreaTypeMedicalPositionsRepository areaTypeMedicalPositionsRepository;

    @Autowired
    private AreaMedicalEmployeeRepository areaMedicalEmployeeRepository;

    @Autowired
    private PositionNomCRUDRepository positionNomCRUDRepository;

    @Autowired
    private BuildingRegistryCRUDRepository buildingRegistryCRUDRepository;

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

    // (К_УУ_1) Добавление типов участков, доступных для МО
    @Override
    public void addMoAvailableAreaTypes(long moId, List<Long> areaTypeCodes) throws ContingentException {
        Validation validation = new Validation();
        List<AreaType> areaTypes = areaHelper.checkAndGetAreaTypesExist(areaTypeCodes, validation, "areaTypeCode");
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

    // (К_УУ_7)	Создание участка обслуживания первичного типа
    @Override @LogESU(type = AreaInfoEvent.class, useResult = true)
    public Long createPrimaryArea(long moId, long muId, Integer muTypeCode, Integer number, Long areaTypeCode, List<Long> policyTypesIds,
                                  Integer ageMin, Integer ageMax, Integer ageMinM, Integer ageMaxM, Integer ageMinW, Integer ageMaxW,
                                  boolean autoAssignForAttachment, Boolean attachByMedicalReason, String description) throws ContingentException {
        Validation validation = new Validation();

        // 1
        AreaType areaType = areaHelper.checkAndGetAreaTypesExist(
                Collections.singletonList(areaTypeCode), validation, "areaTypeCode").get(0);

        // 2
        areaHelper.checkAreaTypeAvailable(areaType, validation);

        // 3
        areaHelper.checkAreaTypeCountLimits(moId, muId, areaType, validation);

        // 4
        if (areaType.getAreaTypeKind() != null &&
                Objects.equals(areaType.getAreaTypeKind().getCode(), AreaTypeKindEnum.MILDLY_ASSOCIATED.getCode()) &&
                (Strings.isNullOrEmpty(description) || number == null ||
                        ageMin == null && ageMax == null && ageMinM == null && ageMaxM == null && ageMinW == null && ageMaxW == null)) {
            validation.error(AreaErrorReason.SOFT_RELATED_AREA_MUST_BE_FILLED);
        }

        // 5
        areaHelper.checkAreaExistsInMU(muId, areaTypeCode, number, null, validation);

        // 6
        areaHelper.checkPolicyTypesIsOMS(policyTypesIds, validation);

        // 7
        areaHelper.checkAreaTypeAgeSetups(areaType, ageMin, ageMax, ageMinM, ageMaxM, ageMinW, ageMaxW, validation);

        // 8
        areaHelper.checkAutoAssignForAttachment(areaType, autoAssignForAttachment, attachByMedicalReason, validation);

        // 9
        areaHelper.checkAttachByMedicalReason(areaType, attachByMedicalReason, validation);

        // 10
        Area area = new Area(moId, muId, areaType, number, autoAssignForAttachment, false, description,
                attachByMedicalReason, ageMin, ageMax, ageMinM, ageMaxM, ageMinW, ageMaxW, LocalDateTime.now());
        areaCRUDRepository.save(area);

        // 11
        List<PolicyType> policyTypes = policyTypeRepository.findByIds(policyTypesIds);
        List<AreaPolicyTypes> areaPolicyTypes = policyTypes.stream().map(policyType ->
                new AreaPolicyTypes(area, policyType)).collect(Collectors.toList());
        areaPolicyTypesCRUDRepository.saveAll(areaPolicyTypes);

        // 12
        //if (areaHelper.isAreaPrimary(area)) {
        //    esuHelperService.sendAreaInfoEvent(area, "createPrimaryArea");
        //}

        // 13
        return area.getId();
    }

    // (К_УУ_8)	Создание участка обслуживания зависимого типа
    @Override
    public Long createDependentArea(long moId, Long muId, Integer number, Long areaTypeCode,
                                    List<Long> primaryAreaTypeCodesIds, List<Long> policyTypeCodesIds,
                                    Integer ageMin, Integer ageMax, Integer ageMinM, Integer ageMaxM, Integer ageMinW, Integer ageMaxW,
                                    boolean autoAssignForAttachment, String description) throws ContingentException {

        Validation validation = new Validation();

        // 1.
        List<AreaType> areaTypes = areaHelper.checkAndGetAreaTypesExist(Collections.singletonList(areaTypeCode), validation, "areaTypeCode");
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        AreaType areaType = areaTypes.get(0);

        // 2.
        areaHelper.checkPrimaryAreasTypesInMUProfile(muId, areaType, validation);

        // 3.
        if (!areaRepository.findAreas(moId, muId, areaTypeCode, null, true).isEmpty()) {
            validation.error(AreaErrorReason.AREA_WITH_TYPE_EXISTS_IN_MO, new ValidationParameter("areaTypeCode", areaTypeCode));
        }
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        // 4. TODO очень странно..
        areaHelper.checkPolicyTypesDepArea(policyTypeCodesIds, validation);
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        // 5.
        areaHelper.checkAreaTypeAgeSetups(areaType, ageMin, ageMax, ageMinM, ageMaxM, ageMinW, ageMaxW, validation);
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        // 6.
        Area area = new Area(moId, muId, areaType, number, autoAssignForAttachment, false, description,
                null, ageMin, ageMax, ageMinM, ageMaxM, ageMinW, ageMaxW, LocalDateTime.now());
        areaCRUDRepository.save(area);

        // 7.
        AreaToAreaType areaToAreaType = new AreaToAreaType();
        areaToAreaType.setArea(area);
        areaToAreaType.setAreaType(areaTypesCRUDRepository.findById(areaTypeCode).get());
        areaToAreaTypeCRUDRepository.save(areaToAreaType);

        // 8.
        List<PolicyType> policyTypeList = policyTypeRepository.findByIds(policyTypeCodesIds);

        if (policyTypeCodesIds != null && !policyTypeCodesIds.isEmpty()) {
            AreaPolicyTypes areaPolicyTypes = new AreaPolicyTypes();
            areaPolicyTypes.setArea(area);
            areaPolicyTypes.setPolicyType(policyTypeList.get(0));
            areaPolicyTypesCRUDRepository.save(areaPolicyTypes);
        }

        // 9.
        List<Area> primAreas = areaRepository.findAreas(moId, muId, primaryAreaTypeCodesIds, null, true);

        // 10.
        primAreas.forEach(primArea ->
                esuHelperService.sendAttachOnAreaChangeEvent(Collections.singletonList(area.getId()),
                        null, primArea)
        );

        // 11.
        return area.getId();
    }

    // (К_УУ_9)	Изменение участка обслуживания первичного типа
    @Override @LogESU(type = AreaInfoEvent.class, parameters = {"areaId"})
    public void updatePrimaryArea(long areaId, Integer number, List<Long> policyTypesAddIds, List<Long> policyTypesDelIds,
                                  Integer ageMin, Integer ageMax, Integer ageMinM, Integer ageMaxM, Integer ageMinW, Integer ageMaxW,
                                  boolean autoAssignForAttachment, Boolean attachByMedicalReason, String description) throws ContingentException {

        Validation validation = new Validation();

        // 1, 2
        Area area = areaHelper.checkAndGetArea(areaId, validation);
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        Area oldArea = historyService.clone(area);

        // 3
        if (number != null) {
            areaHelper.checkAreaExistsInMU(area.getMuId(), area.getAreaType().getCode(), number, area.getId(), validation);
        }

        // 4
        areaHelper.checkPolicyTypesIsOMS(policyTypesAddIds, validation);

        // 5
        List<PolicyType> policyTypesDel = policyTypeRepository.findByIds(policyTypesDelIds);
        areaHelper.checkPolicyTypesDel(area, policyTypesDel, validation);

        // 6
        if (autoAssignForAttachment) {
            if (area.getAreaType().getMpguAvailable() != null
                    && !Boolean.TRUE.equals(area.getAreaType().getMpguAvailable())) {
                validation.error(AreaErrorReason.CANT_SET_AUTO_ASSIGN_FOR_ATTACHMENT,
                        new ValidationParameter("areaTypeCode", area.getAreaType().getCode()));
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
        area.setAutoAssignForAttach(autoAssignForAttachment);
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
                                    Integer ageMin, Integer ageMax, Integer ageMinM, Integer ageMaxM, Integer ageMinW, Integer ageMaxW,
                                    Boolean autoAssignForAttachment) throws ContingentException {

        Validation validation = new Validation();
        //1. и 2.
        Area area = areaHelper.checkAndGetArea(areaId, validation);


        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        Area oldArea = historyService.clone(area);

        // 3.
        if (muId != null && !muId.equals(area.getMuId()) && area.getAreaType().getAreaTypeKind() != null
                && !Objects.equals(area.getAreaType().getAreaTypeKind().getCode(),
                3L)) {
            validation.error(AreaErrorReason.AREA_NOT_RELATED_TO_SPECIAL_OFFICE);
        }
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        // 4.
        primaryAreaTypeCodesAddIds.forEach(pat -> {
            AreaType areaType = areaTypesCRUDRepository.findById(pat).get();
            areaHelper.checkPrimaryAreasTypesInMUProfile(muId, areaType, validation);
            }
        );

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        // 5.
        areaHelper.checkPolicyTypesDepArea(policyTypesAddIds, validation);
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        List<PolicyType> policyTypesAdd = policyTypeRepository.findByIds(policyTypesAddIds);

        // 6.
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

        // 7.
        areaHelper.checkAreaTypeAgeSetups(area.getAreaType(), ageMin, ageMax, ageMinM, ageMaxM, ageMinW, ageMaxW, validation);
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        // 8.
        Long muIdFinal = muId == null ? area.getMuId() : muId;
        area.setMuId(muIdFinal);
        area.setNumber(number == null ? area.getNumber() : number);
        area.setAgeMax(ageMax == null ? area.getAgeMax() : ageMax);
        area.setAgeMin(ageMin == null ? area.getAgeMin() : ageMin);
        area.setAgeMMax(ageMaxM == null ? area.getAgeMMax() : ageMaxM);
        area.setAgeMMin(ageMinM == null ? area.getAgeMMin() : ageMinM);
        area.setAgeWMax(ageMaxW == null ? area.getAgeWMax() : ageMaxW);
        area.setAgeWMin(ageMinW == null ? area.getAgeWMin() : ageMinW);
        area.setAutoAssignForAttach(autoAssignForAttachment == null ? area.getAutoAssignForAttach() : autoAssignForAttachment);
//        area.setAttachByMedicalReason(); TODO нет такого входного параметра
        area.setDescription(description == null ? area.getDescription() : description);
        area.setUpdateDate(LocalDateTime.now());
        areaCRUDRepository.save(area);

        // 9.
        // 9.1.
        primaryAreaTypeCodesAddIds.forEach(pata -> {
            AreaType areaType = areaTypesCRUDRepository.findById(pata).get();
            AreaToAreaType areaToAreaType = new AreaToAreaType();
            areaToAreaType.setArea(area);
            areaToAreaType.setAreaType(areaType);
            areaToAreaTypeCRUDRepository.save(areaToAreaType);
        });
        // 9.2.
        primaryAreaTypeCodesDelIds.forEach(patd -> {
            AreaType areaType = areaTypesCRUDRepository.findById(patd).get();
            List<AreaToAreaType> areaToAreaTypes = areaToAreaTypeRepository.findAreaTypesByAreaAndTypeCode(area, Collections.singletonList(areaType));
            areaToAreaTypeCRUDRepository.deleteAll(areaToAreaTypes);
        });

        // 10.
        areaHelper.saveAndDeleteAreaPolicyTypes(area, policyTypesAdd, policyTypesDel);

        // Вид участка не именной
        if (area.getAreaType() != null && area.getAreaType().getAreaTypeKind() != null &&
                !area.getAreaType().getAreaTypeKind().getCode().equals(4L)) {
            // 11.
            if (!primaryAreaTypeCodesAddIds.isEmpty()) {
                // 11.1.
                List<Area> primAreasAdd = areaRepository.findAreas(area.getMoId(), area.getMuId(), primaryAreaTypeCodesAddIds, null, true);

                // 11.2.
                primAreasAdd.forEach(primArea ->
                        esuHelperService.sendAttachOnAreaChangeEvent(Collections.singletonList(area.getId()),
                                null, primArea)
                );
            }

            // 12.
            if (!primaryAreaTypeCodesDelIds.isEmpty()) {
                // 12.1.
                List<Area> primAreasDel = areaRepository.findAreas(area.getMoId(), area.getMuId(), primaryAreaTypeCodesDelIds, null, true);
                // 12.2.
                primAreasDel.forEach(primArea ->
                        esuHelperService.sendAttachOnAreaChangeEvent(Collections.singletonList(area.getId()),
                                null, primArea)
                );
            }
        }

        // Логирование изменений
        historyService.write(UserContextHolder.getPrincipal(), oldArea, area);

        // 13.
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

        if (!area.getAreaType().getCode().equals(1L) && !area.getAreaType().getCode().equals(3L)) {

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
            Optional<PositionNom> positionNom = positionNomCRUDRepository.findById(empl.getPositionId());

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
                        new ValidationParameter("jobInfoId", empl.getMedicalEmployeeJobInfoId()),
                        new ValidationParameter("AreaSpecialization", area.getAreaType().getCode()));

            }

            // 5.6.
            List<AreaTypeMedicalPositions> positions = areaTypeMedicalPositionsRepository.getPositionsByAreaType(area.getAreaType().getCode());
            if (positions != null && positions.stream().anyMatch(pos -> pos.getPositionNom().getId() != empl.getPositionId())) {
                validation.error(AreaErrorReason.POSITION_NOT_SET_FOR_AREA_TYPE,
                        new ValidationParameter("positionTitle", positionNom.get().getTitle()),
                        new ValidationParameter("jobInfoId", empl.getMedicalEmployeeJobInfoId()),
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
        if (area.getAreaType().getCode() == AreaTypeKindEnum.MILDLY_ASSOCIATED.getCode()) {
            areaHelper.checkMainEmployeesOverlappingDates(mainEmployees, validation);
        }

        //6.3
        if (area.getAreaType().getCode() == AreaTypeKindEnum.MILDLY_ASSOCIATED.getCode()
                || area.getAreaType().getCode() == AreaTypeKindEnum.TREATMENT_ROOM_ASSOCIATED.getCode()) {
            //6.4
            List<Period> periodsWithoutMainEmpl = areaHelper.getPeriodsWithoutMainEmployee(mainEmployees);
            if (periodsWithoutMainEmpl.size() > 0) {
                //6.5
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
    public List<Long> addAreaAddress(Long areaId, List<NsiAddress> nsiAddresses,
                                     List<NotNsiAddress> notNsiAddresses) throws ContingentException {
        Validation validation = new Validation();

        // 1. и 2.
        Area area = areaHelper.checkAndGetArea(areaId, validation);
        if (!validation.isSuccess()) { throw new ContingentException(validation); }

        // 3.
        areaHelper.checkNoAddresses(nsiAddresses, notNsiAddresses);

        // 4.
        areaHelper.checkTooManyAddresses(nsiAddresses, notNsiAddresses, settingService.getPar1());

        // 5.
        areaAddressChecker.checkNsiAddresses(nsiAddresses, validation);

        // 6.
        areaAddressChecker.checkNotNsiAddresses(notNsiAddresses, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        // Список все адресов с привязанными мета-данными
        List<AddressWrapper> addressWrapperList = areaHelper.convertToAddressWarapper(nsiAddresses, notNsiAddresses);

        // 7. Обогащение обертки объектами территорией обслуживания МО
        areaHelper.addMoAddressToAddressWrapper(area, addressWrapperList, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        // 8.
        areaHelper.checkAddressNotServiceByAreatype(area, addressWrapperList, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        // 9.
        addressWrapperList.forEach(addressWrapper -> {
            if (addressWrapper.getNotNsiAddress() != null) {
                NotNsiAddress nna = addressWrapper.getNotNsiAddress();
                Optional<BuildingRegistry> buildingRegistryOptional = buildingRegistryRepository.findRegistryBuildings(
                        nna.getHouse(), nna.getBuilding(), nna.getConstruction(),
                        nna.getParentId()).stream().findFirst();
                if (!buildingRegistryOptional.isPresent()) {
                    BuildingRegistry buildingRegistry = new BuildingRegistry(
                            nna.getParentId(), addressFormingElementRepository.findAfeByIdAndLevel(nna.getParentId(), nna.getLevelParentId()).get(0),
                            nna.getHouseType(), nna.getHouse(),
                            nna.getBuildingType(), nna.getBuilding(),
                            nna.getConstructionType(), nna.getConstruction());
                    addressWrapper.setBuildingRegistry(buildingRegistry);
                } else {
                    addressWrapper.setBuildingRegistry(buildingRegistryOptional.get());
                }
            }
        });

        // 10.
        addressWrapperList.forEach(addressWrapper -> {
            if (addressWrapper.getNsiAddress() != null) {
                // НСИ
                NsiAddress nsiAddress = addressWrapper.getNsiAddress();
                AddressFormingElement addressFormingElement = null;
                BuildingRegistry buildingRegistry = null;

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
                    address.setLevel(nsiAddress.getLevelAddress());
                    address.setAddressFormingElement(addressFormingElement);
                    address.setBuildingRegistry(buildingRegistry);
                    addressWrapper.setAddress(addressesCRUDRepository.save(address));
                }

            } else {
                // неНСИ
                BuildingRegistry buildingRegistry = addressWrapper.getBuildingRegistry();

                List<Addresses> foundAddresses = addressesRepository.findAddresses(8L, buildingRegistry, null);

                if (!foundAddresses.isEmpty()) {
                    addressWrapper.setAddress(foundAddresses.get(0));
                } else {
                    Addresses address = new Addresses();
                    address.setLevel(8);
                    address.setBuildingRegistry(buildingRegistry);
                    addressWrapper.setAddress(addressesCRUDRepository.save(address));
                }

            }

        });

        // 11.
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

        // 12.
        //if (areaHelper.isAreaPrimary(area)) {
        //    esuHelperService.sendAreaInfoEvent(area, "addAreaAddress");
        //}

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
            if (address.getLevel() == 8) {
                BuildingRegistry buildingRegistry = address.getBuildingRegistry();
                AddressFormingElement addressFormingElement = buildingRegistry.getAddressFormingElement();
                addressAreas.add(new moscow.ptnl.contingent.area.model.area.AddressArea(addressId, buildingRegistry, addressFormingElement));
            } else {
                addressAreas.add(new moscow.ptnl.contingent.area.model.area.AddressArea(addressId, address.getAddressFormingElement()));
            }
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
            areaHelper.checkAreaExistsInMU(area.getMuId(), area.getAreaType().getCode(), area.getNumber(), area.getId(), validation);
        }
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        // 5.
        area.setAutoAssignForAttach(false);

        // 6.
        area.setArchived(false);
        areaCRUDRepository.save(area);

        // 7.
        if (areaHelper.isAreaPrimary(area)) {
            // 7.1. Поиск зависимых участков подобного типа
            List<Area> areas = areaRepository.findDependentAreasByAreaEqAreaType(area);


            // 7.2.
            areas.forEach(depArea ->
                    esuHelperService.sendAttachOnAreaChangeEvent(Collections.singletonList(areaId), null, depArea)
            );
        }

        // 8.
        if (areaHelper.isAreaDependent(area)) {
            List<Area> areas = areaRepository.findPrimaryAreasByAreaEqAreaType(area);

            esuHelperService.sendAttachOnAreaChangeEvent(areas.stream().map(Area::getId).collect(Collectors.toList()),
                            null, area);
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
    public List<Long> addMoAddress(long moId, long areaTypeCode, long orderId, List<NsiAddress> nsiAddresses,
                                   List<NotNsiAddress> notNsiAddresses) throws ContingentException {
        Validation validation = new Validation();
        // 1.
        areaHelper.checkNoAddresses(nsiAddresses, notNsiAddresses);
        // 2.
        areaHelper.checkTooManyAddresses(nsiAddresses, notNsiAddresses, settingService.getPar1());
        // 3.
        List<AreaType> areaTypes = areaHelper.checkAndGetAreaTypesExist(Collections.singletonList(areaTypeCode),
                validation, "areaTypeCode");
        // 4.
        AddressAllocationOrders order = addressAllocationOrderCRUDRepository.findById(orderId).orElse(null);

        if (order == null || Boolean.TRUE.equals(order.getArchived())) {
            throw new ContingentException(AreaErrorReason.ADDRESS_ALLOCATION_ORDER_NOT_EXISTS,
                    new ValidationParameter("orderId", orderId));
        }
        // 5.
        areaAddressChecker.checkNsiAddresses(nsiAddresses, validation);
        // 6.
        areaAddressChecker.checkNotNsiAddresses(notNsiAddresses, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        // 7. Система для каждого переданного адреса выполняет поиск пересекающихся распределенных адресов
        for (NsiAddress nsiAddress : nsiAddresses) {
            MoAddress moAddress = algorithms.searchServiceDistrictMOByAddress(moId, areaTypes.get(0), orderId,
                    Collections.singletonList(nsiAddress), Collections.EMPTY_LIST, validation);

            if (moAddress != null) {
                validation.error(AreaErrorReason.ADDRESS_ALREADY_EXISTS_1,
                        new ValidationParameter("levelAddress", nsiAddress.getLevelAddress()),
                        new ValidationParameter("globalId", nsiAddress.getGlobalId()),
                        new ValidationParameter("moId", moAddress.getMoId()),
                        new ValidationParameter("areaTypeCode", moAddress.getAreaType().getCode()));
            }
        }
        for (NotNsiAddress notNsiAddress : notNsiAddresses) {
            MoAddress moAddress = algorithms.searchServiceDistrictMOByAddress(moId, areaTypes.get(0), orderId,
                    Collections.EMPTY_LIST, Collections.singletonList(notNsiAddress), validation);

            if (moAddress != null) {
                String parameters = notNsiAddress.getHouseType() + ": " +
                        notNsiAddress.getHouse() + ", " +
                        notNsiAddress.getBuildingType() + ": " +
                        notNsiAddress.getBuilding() + ", " +
                        notNsiAddress.getConstructionType() + ": " +
                        notNsiAddress.getConstruction();
                validation.error(AreaErrorReason.ADDRESS_ALREADY_EXISTS_2,
                        new ValidationParameter("parentId", notNsiAddress.getParentId()),
                        new ValidationParameter("address", parameters),
                        new ValidationParameter("moId", moAddress.getMoId()),
                        new ValidationParameter("areaTypeCode", moAddress.getAreaType().getCode()));
            }
        }
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        List<AddressWrapper> addresses = areaHelper.convertAddressToWrapper(nsiAddresses, notNsiAddresses);

        addresses.forEach(a -> {
            Addresses address = new Addresses();

            if (a.nsiAddress != null) {
                address.setLevel(a.nsiAddress.getLevelAddress());

                if (a.nsiAddress.getLevelAddress() != AddressLevelType.ID.getLevel()) {
                    address.setAddressFormingElement(a.addressFormingElement);
                }
                else {
                    address.setBuildingRegistry(a.buildingRegistry);
                }
            }
            else {
                // 8.
                address.setLevel(AreaServiceHelper.NOT_NSI_ADDRESS_LEVEL);
                a.buildingRegistry = buildingRegistryRepository.findRegistryBuildings(
                        a.notNsiAddress.getHouse(), a.notNsiAddress.getBuilding(), a.notNsiAddress.getConstruction(),
                        a.notNsiAddress.getParentId()).stream()
                        .findFirst()
                        .orElseGet(() -> {
                            BuildingRegistry buildingRegistry = new BuildingRegistry(
                                    a.notNsiAddress.getParentId(), a.addressFormingElement,
                                    a.notNsiAddress.getHouseType(), a.notNsiAddress.getHouse(),
                                    a.notNsiAddress.getBuildingType(), a.notNsiAddress.getBuilding(),
                                    a.notNsiAddress.getConstructionType(), a.notNsiAddress.getConstruction());
                            buildingRegistryCRUDRepository.save(buildingRegistry);
                            return buildingRegistry;
                        });
                address.setBuildingRegistry(a.buildingRegistry);
            }
            // 9.
            a.address = addressesRepository.findAddresses(address.getLevel(), address.getBuildingRegistry(), address.getAddressFormingElement())
                    .stream().findFirst().orElseGet(() -> addressesCRUDRepository.save(address));
        });
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

        areaHelper.checkAndGetAreaTypesExist(areaTypeCodes, validation, "areaType");

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
