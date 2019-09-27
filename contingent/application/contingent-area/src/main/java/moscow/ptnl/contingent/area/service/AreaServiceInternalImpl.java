package moscow.ptnl.contingent.area.service;

import moscow.ptnl.contingent.area.entity.area.AddressAllocationOrders;
import moscow.ptnl.contingent.area.entity.area.Addresses;
import moscow.ptnl.contingent.area.entity.area.Area;
import moscow.ptnl.contingent.area.entity.area.AreaAddress;
import moscow.ptnl.contingent.area.entity.area.AreaMedicalEmployees;
import moscow.ptnl.contingent.area.entity.area.AreaPolicyTypes;
import moscow.ptnl.contingent.area.entity.area.AreaToAreaType;
import moscow.ptnl.contingent.area.entity.area.MoAddress;
import moscow.ptnl.contingent.area.entity.area.MoAvailableAreaTypes;
import moscow.ptnl.contingent.area.entity.area.MuAvailableAreaTypes;
import moscow.ptnl.contingent.area.error.AreaErrorReason;
import moscow.ptnl.contingent.area.model.area.AddressArea;
import moscow.ptnl.contingent.area.model.area.AreaInfo;
import moscow.ptnl.contingent.area.model.area.AreaTypeStateType;
import moscow.ptnl.contingent.area.model.area.MuAreaTypesFull;
import moscow.ptnl.contingent.area.transform.AddressMapper;
import moscow.ptnl.contingent.area.transform.AddressRegistryBaseTypeMapper;
import moscow.ptnl.contingent.area.transform.AreaAddressClone;
import moscow.ptnl.contingent.area.transform.SearchAreaAddress;
import moscow.ptnl.contingent.area.util.Period;
import moscow.ptnl.contingent.domain.esu.event.AreaInfoEvent;
import moscow.ptnl.contingent.domain.esu.event.annotation.LogESU;
import moscow.ptnl.contingent.error.ContingentException;
import moscow.ptnl.contingent.error.Validation;
import moscow.ptnl.contingent.error.ValidationParameter;
import moscow.ptnl.contingent.nsi.domain.area.AreaType;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeKindEnum;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeMedicalPositions;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeSpecializations;
import moscow.ptnl.contingent.nsi.domain.area.PolicyType;
import moscow.ptnl.contingent.nsi.domain.area.PositionCode;
import moscow.ptnl.contingent.nsi.domain.area.PositionNom;
import moscow.ptnl.contingent.nsi.domain.area.Specialization;
import moscow.ptnl.contingent.nsi.repository.AddressFormingElementRepository;
import moscow.ptnl.contingent.nsi.repository.AreaTypeMedicalPositionsRepository;
import moscow.ptnl.contingent.nsi.repository.AreaTypeSpecializationsRepository;
import moscow.ptnl.contingent.nsi.repository.AreaTypesCRUDRepository;
import moscow.ptnl.contingent.nsi.repository.BuildingRegistryRepository;
import moscow.ptnl.contingent.nsi.repository.PolicyTypeRepository;
import moscow.ptnl.contingent.nsi.repository.PositionCodeRepository;
import moscow.ptnl.contingent.nsi.repository.PositionNomRepository;
import moscow.ptnl.contingent.repository.area.AddressAllocationOrderCRUDRepository;
import moscow.ptnl.contingent.repository.area.AddressAllocationOrderRepository;
import moscow.ptnl.contingent.repository.area.AddressesCRUDRepository;
import moscow.ptnl.contingent.repository.area.AddressesRepository;
import moscow.ptnl.contingent.repository.area.AreaAddressCRUDRepository;
import moscow.ptnl.contingent.repository.area.AreaAddressRepository;
import moscow.ptnl.contingent.repository.area.AreaCRUDRepository;
import moscow.ptnl.contingent.repository.area.AreaMedicalEmployeeCRUDRepository;
import moscow.ptnl.contingent.repository.area.AreaMedicalEmployeeRepository;
import moscow.ptnl.contingent.repository.area.AreaPolicyTypesCRUDRepository;
import moscow.ptnl.contingent.repository.area.AreaPolicyTypesRepository;
import moscow.ptnl.contingent.repository.area.AreaRepository;
import moscow.ptnl.contingent.repository.area.AreaToAreaTypeCRUDRepository;
import moscow.ptnl.contingent.repository.area.AreaToAreaTypeRepository;
import moscow.ptnl.contingent.repository.area.MoAddressCRUDRepository;
import moscow.ptnl.contingent.repository.area.MoAddressRepository;
import moscow.ptnl.contingent.repository.area.MoAvailableAreaTypesCRUDRepository;
import moscow.ptnl.contingent.repository.area.MoAvailableAreaTypesRepository;
import moscow.ptnl.contingent.repository.area.MuAvailableAreaTypesCRUDRepository;
import moscow.ptnl.contingent.repository.area.MuAvailableAreaTypesRepository;
import moscow.ptnl.util.Strings;
import moscow.ptnl.ws.security.UserContextHolder;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import ru.mos.emias.contingent2.address.AddressRegistryBaseType;
import ru.mos.emias.contingent2.area.types.SearchAreaRequest;
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

import static java.util.Comparator.naturalOrder;
import static java.util.Comparator.nullsFirst;

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

    @Autowired
    private AddressMapper addressMapper;

    @Autowired
    private AreaAddressClone areaAddressClone;

    @Autowired
    private HistoryServiceHelper historyHelper;

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
        historyHelper.sendHistory(null, area, Area.class);

        return area.getId();
    }

    // (К_УУ_8)	Создание участка обслуживания зависимого типа
    @Override
    public Long createDependentArea(long moId, Long muId, Integer number, Long areaTypeCode,
                                    List<Long> primaryAreaTypeCodesIds, List<Long> policyTypeCodesIds,
                                    Integer ageMin, Integer ageMax, Integer ageMinM, Integer ageMaxM, Integer ageMinW, Integer ageMaxW,
                                    String description) throws ContingentException {

        Validation validation = new Validation();

        //1
        List<AreaType> areaTypes = areaHelper.checkAndGetAreaTypesExist(Collections.singletonList(areaTypeCode), validation);
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        AreaType areaType = areaTypes.get(0);

        //2
        areaHelper.checkAreaTypeIsDependent(areaType, validation);
        //3
        primaryAreaTypeCodesIds = primaryAreaTypeCodesIds.stream().distinct().collect(Collectors.toList());

        primaryAreaTypeCodesIds.forEach(code -> {
            // TODO позже добавить проверку что primaryAreaTypeCode есть в таблице areaTypes
            Optional<AreaType> primaryAreaTypeOptional = areaTypesCRUDRepository.findById(code);
            if (!primaryAreaTypeOptional.isPresent()) {
                validation.error(AreaErrorReason.AREA_TYPE_NOT_FOUND, new ValidationParameter("areaType", code));
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
        //7
        if (!areaRepository.findAreas(moId, muId, areaTypeCode, null, true).isEmpty()) {
            validation.error(AreaErrorReason.AREA_WITH_TYPE_EXISTS_IN_MO, new ValidationParameter("areaTypeCode", areaTypeCode));
        }
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        //8
        areaHelper.checkPolicyTypesIsOMS(policyTypeCodesIds, validation);

        //9
        areaHelper.checkAreaTypeAgeSetups(areaType, ageMin, ageMax, ageMinM, ageMaxM, ageMinW, ageMaxW, validation);
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        //10
        Area area = new Area(moId, muId, areaType, number, null, false, description,
                null, ageMin, ageMax, ageMinM, ageMaxM, ageMinW, ageMaxW, LocalDateTime.now());
        areaCRUDRepository.save(area);

        //11.
        for (Long areaTypeCodeId : primaryAreaTypeCodesIds) {
            AreaToAreaType areaToAreaType = new AreaToAreaType();
            areaToAreaType.setArea(area);
            areaToAreaType.setAreaType(areaTypesCRUDRepository.findById(areaTypeCodeId).get());
            areaToAreaTypeCRUDRepository.save(areaToAreaType);
        }
        //12
        List<PolicyType> policyTypeList = policyTypeRepository.findByIds(policyTypeCodesIds);

        if (policyTypeCodesIds != null && !policyTypeCodesIds.isEmpty()) {
            AreaPolicyTypes areaPolicyTypes = new AreaPolicyTypes();
            areaPolicyTypes.setArea(area);
            areaPolicyTypes.setPolicyType(policyTypeList.get(0));
            areaPolicyTypesCRUDRepository.save(areaPolicyTypes);
        }

        //13
        List<Area> primAreas = areaRepository.findAreas(moId, muId, primaryAreaTypeCodesIds, null, true);

        //14
        esuHelperService.sendAttachOnAreaChangeEvent(primAreas.stream().map(Area::getId).collect(Collectors.toList()),
                null, area);

        //15
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
        areaCRUDRepository.save(area);

        // 10
        areaHelper.saveAndDeleteAreaPolicyTypes(area, policyTypesAdd, policyTypesDel);

        // Логирование изменений
        historyHelper.sendHistory(oldArea, area, Area.class);
    }

    // (К_УУ_10) Изменение участка обслуживания зависимого типа
    @Override
    public void updateDependentArea(long areaId, Long muId, Integer number, String description,
                                    List<Long> primaryAreaTypeCodesAddIds, List<Long> primaryAreaTypeCodesDelIds,
                                    List<Long> policyTypesAddIds, List<Long> policyTypesDelIds,
                                    Integer ageMin, Integer ageMax, Integer ageMinM, Integer ageMaxM, Integer ageMinW, Integer ageMaxW) throws ContingentException {

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
                !Objects.equals(area.getAreaType().getAreaTypeKind().getCode(), AreaTypeKindEnum.PERSONAL.getCode()))) {
            validation.error(AreaErrorReason.AREA_NOT_RELATED_TO_SPECIAL_OFFICE);
        }
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        // 6.
        primaryAreaTypeCodesAddIds = primaryAreaTypeCodesAddIds.stream().distinct().collect(Collectors.toList());
        primaryAreaTypeCodesDelIds = primaryAreaTypeCodesDelIds.stream().distinct().collect(Collectors.toList());

        primaryAreaTypeCodesAddIds.forEach(pat -> {
            // TODO позже добавить проверку что primaryAreaTypeCode есть в таблице areaTypes
            AreaType primaryAreaType = areaTypesCRUDRepository.findById(pat).get();
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
                            new ValidationParameter("areTypeTitle", areaTypesCRUDRepository.findById(areaTypeToDelete).get().getTitle()));
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
//        area.setAttachByMedicalReason(); TODO нет такого входного параметра
        area.setDescription(description == null ? area.getDescription() : description);
        area.setUpdateDate(LocalDateTime.now());
        areaCRUDRepository.save(area);

        // 13.
        // 13.1.
        primaryAreaTypeCodesAddIds.forEach(pata -> {
            AreaType areaType = areaTypesCRUDRepository.findById(pata).get();
            AreaToAreaType areaToAreaType = new AreaToAreaType();
            areaToAreaType.setArea(area);
            areaToAreaType.setAreaType(areaType);
            areaToAreaTypeCRUDRepository.save(areaToAreaType);
        });
        // 13.2.
        if (!primaryAreaTypeCodesDelIds.isEmpty()) {
            areaToAreaTypeCRUDRepository.deleteAll(areaTypeToDeleteBd);
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
            Optional<PositionNom> positionNomOptional = positionNomRepository.getByPositionCode(positionCode.getCode());

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
                        new ValidationParameter("InputSpecialization", specialization.getTitle()),
                        new ValidationParameter("jobInfoId", empl.getMedicalEmployeeJobId()),
                        new ValidationParameter("AreaSpecialization", area.getAreaType().getCode()));

            }

            // 5.6.            
            List<AreaTypeMedicalPositions> positions = areaTypeMedicalPositionsRepository.getPositionsByAreaType(area.getAreaType().getCode());

            if (positions != null && positions.stream().noneMatch(pos -> pos.getPositionCode().getCode().equals(empl.getPositionCode()))) {
                validation.error(AreaErrorReason.POSITION_NOT_SET_FOR_AREA_TYPE,
                        new ValidationParameter("positionTitle", positionNom.getTitle()),
                        new ValidationParameter("jobInfoId", empl.getMedicalEmployeeJobId()),
                        new ValidationParameter("areaTypeName", area.getAreaType().getTitle()));
            }
        }
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        //6.1
        List<AreaMedicalEmployees> allEmployees = new ArrayList<>(areaEmployeesDb);
        Map<AreaMedicalEmployees, AreaMedicalEmployees> changesAme =  areaHelper.applyChanges(allEmployees, changeEmployeesInput);
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
                replacementEmployees.sort(Comparator.comparing(AreaMedicalEmployees::getStartDate, nullsFirst(naturalOrder())));
                areaHelper.checkReplacementWithoutMain(periodsWithoutMainEmpl, replacementEmployees, validation);
            }
        }

        //7
        List<Long> result = new ArrayList<>();
        areaHelper.applyChanges(changeEmployeesDb, changeEmployeesInput);
        areaHelper.addNew(changeEmployeesDb, addEmployeesInput, area);
        areaMedicalEmployeeCRUDRepository.saveAll(changeEmployeesDb).forEach(saved -> {
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
    public List<Long> addAreaAddress(Long areaId, List<AddressRegistryBaseType> addressesRegistry) throws ContingentException {

        Validation validation = new Validation();

        // 1. и 2.
        Area area = areaHelper.checkAndGetArea(areaId, validation);
        if (!validation.isSuccess()) { throw new ContingentException(validation); }

        // 3.
        areaHelper.checkTooManyAddresses(addressesRegistry, settingService.getPar1());

        // 4.
        algorithms.checkAddressFLK(addressesRegistry, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        // 6.
        List<MoAddress> findMoAddress = new ArrayList<>();
        addressesRegistry.forEach(ar -> {
            MoAddress moAddressInterspect = algorithms.searchServiceDistrictMOByAddress(area.getMoId(), area.getAreaType(), null,
                    addressesRegistry, validation);
            if (moAddressInterspect == null || !moAddressInterspect.getMoId().equals(area.getMoId())) {
                validation.error(AreaErrorReason.ADDRESS_NOT_SERVICED_MO_NSI, new ValidationParameter("addressString",  ar.getAddressString()),
                        new ValidationParameter("moId", area.getMoId()));
            } else if (moAddressInterspect.getMoId().equals(area.getMoId())) {
                findMoAddress.add(moAddressInterspect);
            }
        });

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        // 7.
        addressesRegistry.forEach(ar -> {
            Long areaIdInterspect = algorithms.searchAreaByAddress(area.getMoId(), area.getAreaType(), addressesRegistry, validation);
            if (areaIdInterspect != null) {
                if (!areaIdInterspect.equals(area.getId())) {
                    validation.error(AreaErrorReason.ADDRESS_ALREADY_SERVICED_ANOTHER_AREA, new ValidationParameter("areaType", ar.getArea().getCode()));
                } else {
                    validation.error(AreaErrorReason.ADDRESS_ALREADY_SERVICED_NSI, new ValidationParameter("addressString", ar.getAddressString()));
                }
            }
        });
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        // 8.
        List<Addresses> addresses = addressesRegistry.stream().map(addressMapper::dtoToEntityTransform)
                .collect(Collectors.toList());
        addressesCRUDRepository.saveAll(addresses);

        // 9.
        List<AreaAddress> areaAddresses = addresses.stream().map(addr -> {
            AreaAddress areaAddress = new AreaAddress();
            areaAddress.setArea(area);
            areaAddress.setMoAddress(findMoAddress.get(0));
            areaAddress.setAddress(addr);
            areaAddress.setCreateDate(LocalDateTime.now());
            areaAddress.setUpdateDate(LocalDateTime.now());
            return areaAddress;
        }).collect(Collectors.toList());
        areaAddressCRUDRepository.saveAll(areaAddresses);

        // 10. аннотация @LogESU

        //
        for (AreaAddress areaAddress: areaAddresses) {
            historyHelper.sendHistory(null, areaAddress, AreaAddress.class);
        }

        return areaAddresses.stream().map(AreaAddress::getId).collect(Collectors.toList());
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
            if (areaAddress.getStartDate() == null || !areaAddress.getStartDate().equals(localDate)) {
                AreaAddress areaAddressOld = areaAddressClone.clone(areaAddress);
                // 5.1.
                areaAddress.setEndDate(localDate.minusDays(1L));
                areaAddressCRUDRepository.save(areaAddress);
                historyHelper.sendHistory(areaAddressOld, areaAddress, AreaAddress.class);
            } else {
                historyHelper.sendHistory(areaAddress, null, AreaAddress.class);

                // 5.2.
                areaAddressCRUDRepository.delete(areaAddress);
            }
        }

        // 6. @LogEsu

        // 7.
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
            return new PageImpl<>(new ArrayList<>(), paging, 0);
        }

        // 3.
        TreeMap<Long, Addresses> addresses = new TreeMap<Long, Addresses>();
        areaAddresses.forEach(aa -> {
            addresses.put(aa.getArea().getId(), aa.getAddress());
        });

        // 4.
        // TODO: https://wiki.emias.mos.ru/pages/viewpage.action?pageId=71674265 п.4. НЕРЕАЛИЗУЕМО
        // Сделано без объединения
        List<moscow.ptnl.contingent.area.model.area.AddressArea> addressAreas = new ArrayList<>();
        addresses.forEach((id, addr) -> {
            moscow.ptnl.contingent.area.model.area.AddressArea addressArea = new AddressArea();
            addressArea.setAreaAddressId(id);
            addressArea.setAddresses(addr);
            addressAreas.add(addressArea);
        });

        // 5.
        return new PageImpl<>(new ArrayList<>(addressAreas),
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

        // 9. @LogESU

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
    public void updateOrder(Long id, String number, LocalDate date,
                            String ouz, String name) throws ContingentException {
        Validation validation = new Validation();

        // 1.
        AddressAllocationOrders order = addressAllocationOrderCRUDRepository.findById(id).orElse(null);

        // 2.
        if (order == null) {
            validation.error(AreaErrorReason.ADDRESS_ALLOCATION_ORDER_NOT_EXISTS,
                    new ValidationParameter("id", id));
            throw new ContingentException(validation);
        }
        if (Boolean.TRUE.equals(order.getArchived())) {
            validation.error(AreaErrorReason.ADDRESS_ALLOCATION_ORDER_IS_ARCHIVED,
                    new ValidationParameter("id", id));
        }
        AddressAllocationOrders oldOrder = historyHelper.clone(order);

        // 3.
        if (number == null && date == null && ouz == null && name == null) {
            throw new ContingentException(AreaErrorReason.NOTHING_TO_CHANGE);
        }

        // 4.
        if (Objects.deepEquals(order.getNumber(), number) &&
                Objects.deepEquals(order.getDate(), date) &&
                Objects.deepEquals(order.getOuz(), ouz) &&
                Objects.deepEquals(order.getName(), name)) {
            throw new ContingentException(AreaErrorReason.NOTHING_TO_CHANGE);
        }

        //5
        if (date != null) {
            areaHelper.checkDateTillToday(date, validation);
        }

        //6
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

        //7
        order.setUpdateDate(LocalDateTime.now());
        order.setNumber(numberNew);
        order.setDate(dateNew);
        order.setOuz(ouzNew);
        order.setName(nameNew);

        addressAllocationOrderCRUDRepository.save(order);

        historyHelper.sendHistory(oldOrder, order, AddressAllocationOrders.class);
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
    public List<Long> addMoAddress(long moId, long areaTypeCode, long orderId, List<AddressRegistryBaseType> addressesRegistry)
            throws ContingentException {
        Validation validation = new Validation();
        // 1.
        areaHelper.checkTooManyAddresses(addressesRegistry, settingService.getPar1());

        // 2.
        List<AreaType> areaTypes = areaHelper.checkAndGetAreaTypesExist(Collections.singletonList(areaTypeCode), validation);
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        AreaType areaType = areaTypes.get(0);

        // 3.
        AddressAllocationOrders order = addressAllocationOrderCRUDRepository.findById(orderId).orElse(null);

        if (order == null || Boolean.TRUE.equals(order.getArchived())) {
            throw new ContingentException(AreaErrorReason.ADDRESS_ALLOCATION_ORDER_NOT_EXISTS,
                    new ValidationParameter("orderId", orderId));
        }

        // 4.
        algorithms.checkAddressFLK(addressesRegistry, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        // 5.
        addressesRegistry.forEach(addr -> {
            MoAddress moAddress = algorithms.searchServiceDistrictMOByAddress(moId, areaType, orderId,
                    Collections.singletonList(addr), validation);
            if (moAddress != null) {
                validation.error(AreaErrorReason.ADDRESS_ALREADY_EXISTS, new ValidationParameter("moId", moAddress.getId()));
            }
        });
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        // 6.
        List<Addresses> addresses = addressesRegistry.stream().map(addressMapper::dtoToEntityTransform)
                .collect(Collectors.toList());
        addressesCRUDRepository.saveAll(addresses);

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
            moAddressCRUDRepository.save(moAddress);
            moAddresses.add(moAddress);
        });

        return moAddresses.stream().map(MoAddress::getId).collect(Collectors.toList());
    }

    // (К_УУ_22) Отмена распределения жилых домов к территории обслуживания МО
    @Override
    public void delMoAddress(List<Long> moAddressIds, long orderId) throws ContingentException {
        Validation validation = new Validation();

        // 1.
        if (moAddressIds.size() > settingService.getPar2()) {
            validation.error(AreaErrorReason.TOO_MANY_ADDRESSES, new ValidationParameter("moAddressId", settingService.getPar2()));
        }

        // 2.
        List<MoAddress> addresses = areaHelper.getAndCheckMoAddressesExist(moAddressIds, validation);

        // 3.
        areaHelper.checkOrderExists(orderId, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        // 4.
        List<Long> moAddressesIds = addresses.stream()
                .map(MoAddress::getId)
                .collect(Collectors.toList());
        List<AreaAddress> areaAddresses = areaAddressRepository.findAreaAddresses(moAddressesIds);

        if (!areaAddresses.isEmpty()) {
            areaHelper.deleteAreaAddress(areaAddresses);
        }

        // 5.
        areaHelper.delMoAddresses(addresses);
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

        // 2.
        areaHelper.checkAndGetAreaTypesExist(areaTypeCodes, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        /* TODO https://wiki.emias.mos.ru/pages/viewpage.action?pageId=71674475 п.4. НЕРЕАЛИЗУЕМ */

        return moAddressRepository.getActiveMoAddresses(moId, areaTypeCodes, paging);
    }

    // (К_УУ_24) Получение идентификатора для создания нового участка
    @Override
    public Long getNewAreaId() throws ContingentException {
        return areaRepository.getNextAreaId();
    }

    // (К_УУ_25) Предоставление списка участков
    @Override
    public Page<AreaInfo> searchArea(Long areaTypeClassCode, Long moId, List<Long> muIds, List<Long> areaTypeCodes,
                                     Integer number, String description, Boolean isArchived,
                                     List<SearchAreaRequest.MedicalEmployee> medicalEmployees,
                                     List<SearchAreaAddress> searchAreaAddresses, Boolean isExactAddressMatch,
                                     PageRequest paging) throws ContingentException {
        //1
        areaHelper.checkSearchParameters(areaTypeClassCode, moId, muIds, areaTypeCodes, number, description,
                isArchived, medicalEmployees, searchAreaAddresses);
        //2
        areaHelper.checkSearchAreaAddresses(searchAreaAddresses);

        //3.1
        List<Area> areas = areaRepository.findAreas(areaTypeClassCode, moId, muIds, areaTypeCodes, number, description, isArchived);

        //3.2
        if (!medicalEmployees.isEmpty()) {
            areas = areaMedicalEmployeeRepository.findAreas(areas.stream().map(Area::getId).collect(Collectors.toList()),
                    medicalEmployees.stream().map(SearchAreaRequest.MedicalEmployee::getMedicalEmployeeJobId).collect(Collectors.toList()),
                    medicalEmployees.stream().map(SearchAreaRequest.MedicalEmployee::getSnils).collect(Collectors.toList()));
        }

        //3.3
        if (!searchAreaAddresses.isEmpty()) {
            //3.3.1
            List<AreaAddress> areaAddresses = areaAddressRepository.findActualAreaAddress();
            List<Addresses> addresses;
            if (!areaAddresses.isEmpty()) {
                //3.3.2
                if (isExactAddressMatch == null || isExactAddressMatch) {
                    addresses = addressesRepository.findAddresses(areaAddresses.stream().map(AreaAddress::getId).collect(Collectors.toList()),
                            searchAreaAddresses.stream().map(SearchAreaAddress::getGlobalIdNsi).collect(Collectors.toList()));
                    //3.3.3
                } else {
                    addresses = algorithms.findIntersectingAddressesSearch(areaAddresses.stream().map(AreaAddress::getAddress).collect(Collectors.toList()),
                            searchAreaAddresses.stream().map(AddressRegistryBaseTypeMapper::entityToDtoTransform).collect(Collectors.toList()));
                }
                //3.3.4
                if (!addresses.isEmpty()) {
                    areaAddresses = areaAddressRepository.findAreaAddressByAddressIds(addresses.stream().map(Addresses::getId).collect(Collectors.toList()));
                    List<Long> areaIds = areaAddresses.stream().map(areaAddress -> areaAddress.getArea().getId()).collect(Collectors.toList());
                    areas = areas.stream().filter(area -> areaIds.contains(area.getId())).collect(Collectors.toList());
                }
            }
        }

        int totalSize = areas.size();
        List<AreaInfo> areaInfos = areas.stream().sorted(Comparator.comparingLong(Area::getId))
                .skip(paging.getPageNumber() * paging.getPageSize()).limit(paging.getPageSize())
                .map(AreaInfo::new).collect(Collectors.toList());
        return new PageImpl<>(new ArrayList<>(areaInfos),
                paging, totalSize);
    }



}
