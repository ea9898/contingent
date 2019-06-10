package moscow.ptnl.contingent.area.service;

import moscow.ptnl.contingent.area.entity.area.AddressAllocationOrders;
import moscow.ptnl.contingent.area.entity.area.Addresses;
import moscow.ptnl.contingent.area.entity.area.Area;
import moscow.ptnl.contingent.area.entity.area.AreaAddress;
import moscow.ptnl.contingent.area.entity.area.AreaMedicalEmployees;
import moscow.ptnl.contingent.area.entity.area.AreaToAreaType;
import moscow.ptnl.contingent.area.entity.area.MoAddress;
import moscow.ptnl.contingent.area.entity.area.MuAddlAreaTypes;
import moscow.ptnl.contingent.area.entity.nsi.AddressFormingElement;
import moscow.ptnl.contingent.area.entity.nsi.AreaType;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeMedicalPositions;
import moscow.ptnl.contingent.area.entity.nsi.BuildingRegistry;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeKindEnum;
import moscow.ptnl.contingent.area.entity.nsi.MuTypeAreaTypes;
import moscow.ptnl.contingent.area.entity.nsi.PositionNom;
import moscow.ptnl.contingent.area.entity.nsi.Specialization;
import moscow.ptnl.contingent.area.error.AreaErrorReason;
import moscow.ptnl.contingent.area.error.ContingentException;
import moscow.ptnl.contingent.area.error.Validation;
import moscow.ptnl.contingent.area.error.ValidationParameter;
import moscow.ptnl.contingent.area.model.area.AddressLevelType;
import moscow.ptnl.contingent.area.model.area.AddressWrapper;
import moscow.ptnl.contingent.area.model.area.AreaInfo;
import moscow.ptnl.contingent.area.model.nsi.AvailableToCreateType;
import moscow.ptnl.contingent.area.repository.area.AddressAllocationOrderCRUDRepository;
import moscow.ptnl.contingent.area.repository.area.AddressAllocationOrderRepository;
import moscow.ptnl.contingent.area.repository.area.AddressesCRUDRepository;
import moscow.ptnl.contingent.area.repository.area.AddressesRepository;
import moscow.ptnl.contingent.area.repository.area.AreaAddressCRUDRepository;
import moscow.ptnl.contingent.area.repository.area.AreaAddressRepository;
import moscow.ptnl.contingent.area.repository.area.AreaCRUDRepository;
import moscow.ptnl.contingent.area.repository.area.AreaMedicalEmployeeCRUDRepository;
import moscow.ptnl.contingent.area.repository.area.AreaMedicalEmployeeRepository;
import moscow.ptnl.contingent.area.repository.area.AreaRepository;
import moscow.ptnl.contingent.area.repository.area.AreaToAreaTypeCRUDRepository;
import moscow.ptnl.contingent.area.repository.area.AreaToAreaTypeRepository;
import moscow.ptnl.contingent.area.repository.area.MoAddressCRUDRepository;
import moscow.ptnl.contingent.area.repository.area.MoAddressRepository;
import moscow.ptnl.contingent.area.repository.area.MuAddlAreaTypesCRUDRepository;
import moscow.ptnl.contingent.area.repository.area.MuAddlAreaTypesRepository;
import moscow.ptnl.contingent.area.repository.nsi.AddressFormingElementRepository;
import moscow.ptnl.contingent.area.repository.nsi.AreaTypeMedicalPositionsRepository;
import moscow.ptnl.contingent.area.repository.nsi.AreaTypesCRUDRepository;
import moscow.ptnl.contingent.area.repository.nsi.MuTypeAreaTypesRepository;
import moscow.ptnl.contingent.area.repository.nsi.PositionNomClinicCRUDRepository;
import moscow.ptnl.contingent.area.repository.nsi.BuildingRegistryCRUDRepository;
import moscow.ptnl.contingent.area.repository.nsi.BuildingRegistryRepository;
import moscow.ptnl.contingent.area.repository.nsi.SpecializationToPositionNomRepository;
import moscow.ptnl.contingent.area.util.Period;
import moscow.ptnl.contingent2.area.info.Address;
import moscow.ptnl.util.Strings;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Component;
import ru.mos.emias.contingent2.core.AddMedicalEmployee;
import ru.mos.emias.contingent2.core.ChangeMedicalEmployee;
import ru.mos.emias.contingent2.core.MuType;
import ru.mos.emias.contingent2.core.NotNsiAddress;
import ru.mos.emias.contingent2.core.NsiAddress;

import javax.ejb.Stateless;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.TreeMap;
import java.util.stream.Collectors;

@Component
public class AreaServiceInternalImpl implements AreaServiceInternal {

    @Autowired
    private MuAddlAreaTypesRepository muAddlAreaTypesRepository;

    @Autowired
    private MuAddlAreaTypesCRUDRepository muAddlAreaTypesCRUDRepository;

    @Autowired
    private MuTypeAreaTypesRepository muTypeAreaTypesRepository;

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
    private PositionNomClinicCRUDRepository positionNomCRUDRepository;

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
    Algorithms algorithms;

    // (К_УУ_1)	Добавление типов участка в профиль МУ
    @Override
    public void addProfileMU(Long muId, Long muTypeId, List<Long> areaTypeCodes) throws ContingentException {
        Validation validation = new Validation();

        // 1.
        List<AreaType> areaTypes = areaHelper.checkAndGetAreaTypesExist(areaTypeCodes, validation, "areaTypesAdd");

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        // 2.
        areaHelper.checkMuAddlAreaTypeExist(muId, areaTypeCodes, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        // 3.
        areaHelper.checkMuTypeAreaTypeCreateAvailable(muTypeId, areaTypeCodes, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        // 4.
        for (Long areaTypeCode : areaTypeCodes) {
            AreaType areaType = areaTypes.stream().filter(at -> at.getCode().equals(areaTypeCode)).findFirst().orElse(null);
            MuAddlAreaTypes muAddlAreaTypes = new MuAddlAreaTypes(muId, areaType);
            muAddlAreaTypesCRUDRepository.save(muAddlAreaTypes);
        }

        // 5.
    }

    // (К_УУ_2)	Удаление типов участка из профиля МУ
    @Override
    public void delProfileMU(Long muId, List<Long> areaTypeCodes) throws ContingentException {
        Validation validation = new Validation();

        areaHelper.checkMuProfilesHasAreaTypes(muId, areaTypeCodes, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        areaHelper.checkMuActiveAreasNotExist(muId, areaTypeCodes, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        List<MuAddlAreaTypes> muAddlAreaTypes = muAddlAreaTypesRepository.getMuAddlAreaTypes(muId);
        List<MuAddlAreaTypes> profilesToDelete = muAddlAreaTypes.stream()
                .filter(m -> m.getAreaType() != null
                        && areaTypeCodes.contains(m.getAreaType().getCode()))
                .collect(Collectors.toList());

        if (!profilesToDelete.isEmpty()) {
            muAddlAreaTypesCRUDRepository.deleteAll(profilesToDelete);
        }

    }

    // (К_УУ_3)	Предоставление сведений профиля МУ
    @Override
    public List<AreaType> getProfileMU(long muId, long muTypeId) throws ContingentException {
        Set<AreaType> areaTypes = muTypeAreaTypesRepository.findMuTypeAreaTypes(muTypeId, new ArrayList<>(),
                AvailableToCreateType.ALLOWED).stream()
                .map(MuTypeAreaTypes::getAreaType)
                .collect(Collectors.toSet());
        areaTypes.addAll(muAddlAreaTypesRepository.getMuAddlAreaTypes(muId).stream()
                .map(MuAddlAreaTypes::getAreaType)
                .collect(Collectors.toSet())
        );
        return new ArrayList<>(areaTypes);
    }



    @Override
    public Long createPrimaryArea(long moId, long muId, Integer number, Long areaTypeCode,
                                  Integer ageMin, Integer ageMax, Integer ageMinM, Integer ageMaxM, Integer ageMinW, Integer ageMaxW,
                                  boolean autoAssignForAttachment, Boolean attachByMedicalReason, String description) throws ContingentException {
        Validation validation = new Validation();

        areaHelper.checkAndGetAreaTypesExist(Collections.singletonList(areaTypeCode), validation, "areaTypeCode");

        MuAddlAreaTypes muAddlAreaTypes = muAddlAreaTypesRepository.getMuAddlAreaTypes(muId).stream()
                .filter(p -> p.getAreaType() != null && Objects.equals(p.getAreaType().getCode(), areaTypeCode))
                .findFirst().orElse(null);

        if (muAddlAreaTypes == null) {
            validation.error(AreaErrorReason.MU_PROFILE_HAS_NO_AREA_TYPE, new ValidationParameter("areaTypeCode", areaTypeCode));
        }
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        // TODO аттрибуты переехали. Переделать.
//        Boolean mpguAvailable = muAddlAreaTypes.getAreaType().getAttributes() == null ? null : muAddlAreaTypes.getAreaType().getAttributes().getMpguAvailable();
//        Boolean areaTypeAttachByMedicalReason = muAddlAreaTypes.getAreaType().getAttributes() == null ? null : muAddlAreaTypes.getAreaType().getAttributes().getAttachByMedicalReason();
        //Todo сделать проерку AREA_COUNT_LIMIT после разработки НСИ
        if (muAddlAreaTypes.getAreaType().getKindAreaType() != null &&
                Objects.equals(muAddlAreaTypes.getAreaType().getKindAreaType().getCode(), AreaTypeKindEnum.MILDLY_ASSOCIATED.getCode())) {
            if (Strings.isNullOrEmpty(description) || number == null ||
                    (ageMin == null && ageMax == null && ageMinM == null && ageMaxM == null && ageMinW == null && ageMaxW == null)) {
                validation.error(AreaErrorReason.SOFT_RELATED_AREA_MUST_BE_FILLED);
            }
        }
        areaHelper.checkAreaExistsInMU(muId, areaTypeCode, number, null, validation);
        areaHelper.checkAreaTypeAgeSetups(muAddlAreaTypes.getAreaType(), ageMin, ageMax, ageMinM, ageMaxM, ageMinW, ageMaxW, validation);

//        if (autoAssignForAttachment) {
//            if (!Boolean.TRUE.equals(mpguAvailable)) {
//                validation.error(AreaErrorReason.CANT_SET_AUTO_ASSIGN_FOR_ATTACHMENT, new ValidationParameter("areaTypeCode", areaTypeCode));
//            }
//            if (Boolean.TRUE.equals(attachByMedicalReason)) {
//                validation.error(AreaErrorReason.AREA_FLAGS_INCORRECT);
//            }
//        }
//        if (attachByMedicalReason != null && areaTypeAttachByMedicalReason != null &&
//                !Objects.equals(attachByMedicalReason, areaTypeAttachByMedicalReason)) {
//            validation.error(AreaErrorReason.ATTACH_BY_MEDICAL_REASON_INCORRECT,
//                    new ValidationParameter("attachByMedicalReason", attachByMedicalReason),
//                    new ValidationParameter("attachByMedicalReason", areaTypeAttachByMedicalReason));
//        }
//        if (!validation.isSuccess()) {
//            throw new ContingentException(validation);
//        }
        //Создание новго первичного участка
        Area area = new Area(moId, muId, muAddlAreaTypes.getAreaType(), number, autoAssignForAttachment, false, description,
                attachByMedicalReason, ageMin, ageMax, ageMinM, ageMaxM, ageMinW, ageMaxW, LocalDateTime.now());
        areaCRUDRepository.save(area);

        areaHelper.resetAutoAssignForAttachment(area);

        if (areaHelper.isAreaPrimary(area)) {
            esuHelperService.sendAreaInfoEventTopicToESU(algorithms.createTopicAreaInfo(area, "createPrimaryArea"));
        }

        return area.getId();
    }

    @Override
    public Long createDependentArea(long moId, Long muId, List<MuType> muTypes, Integer number, Long areaTypeCode, List<Long> primaryAreaTypeCodes,
                                    Integer ageMin, Integer ageMax, Integer ageMinM, Integer ageMaxM, Integer ageMinW, Integer ageMaxW,
                                    boolean autoAssignForAttachment, String description) throws ContingentException {
        Validation validation = new Validation();

        areaHelper.checkAndGetAreaTypesExist(Collections.singletonList(areaTypeCode), validation, "areaTypeCode");
        areaHelper.checkPrimaryAreaTypesForMuType(muTypes, primaryAreaTypeCodes, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        if (!areaRepository.findAreas(moId, muId, areaTypeCode, null, true).isEmpty()) {
            validation.error(AreaErrorReason.AREA_WITH_TYPE_EXISTS_IN_MO, new ValidationParameter("areaTypeCode", areaTypeCode));
        }
        AreaType areaType = areaTypesCRUDRepository.findById(areaTypeCode).get();
        areaHelper.checkAreaTypeAgeSetups(areaType, ageMin, ageMax, ageMinM, ageMaxM, ageMinW, ageMaxW, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        Map<Long, AreaType> primaryAreaTypes = areaTypesCRUDRepository.findAreaTypesByCode(primaryAreaTypeCodes).stream()
                .collect(Collectors.toMap(AreaType::getCode, t -> t));
        //Создание новго зависимого участка
        Area area = new Area(moId, muId, areaType, number, autoAssignForAttachment, false, description,
                null, ageMin, ageMax, ageMinM, ageMaxM, ageMinW, ageMaxW, LocalDateTime.now());
        areaCRUDRepository.save(area);
        //Сохранение привязки к первичным типам участка
        primaryAreaTypeCodes.forEach(c -> {
            AreaToAreaType areaToAreaType = new AreaToAreaType();
            areaToAreaType.setArea(area);
            areaToAreaType.setAreaType(primaryAreaTypes.get(c));
            areaToAreaTypeCRUDRepository.save(areaToAreaType);
        });
//        esuHelperService.trySendDependentAreaChange(area);

        return area.getId();
    }

    @Override
    public void updatePrimaryArea(long areaId, Integer number,
                                  Integer ageMin, Integer ageMax, Integer ageMinM, Integer ageMaxM, Integer ageMinW, Integer ageMaxW,
                                  boolean autoAssignForAttachment, Boolean attachByMedicalReason, String description) throws ContingentException {
        Validation validation = new Validation();
        Area area = areaHelper.checkAndGetArea(areaId, validation);
        Area oldArea = new Area(area);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        // TODO аттрибуты переехали в AREA
        // TODO переделать
//        Boolean mpguAvailable = area.getAreaType().getAttributes() == null ? null : area.getAreaType().getAttributes().getMpguAvailable();
//        Boolean areaTypeAttachByMedicalReason = area.getAreaType().getAttributes() == null ? null : area.getAreaType().getAttributes().getAttachByMedicalReason();

        if (number != null) {
            areaHelper.checkAreaExistsInMU(area.getMuId(), area.getAreaType().getCode(), number, area.getId(), validation);
        }
//        if (autoAssignForAttachment) {
//            if (!Boolean.TRUE.equals(mpguAvailable)) {
//                validation.error(AreaErrorReason.CANT_SET_AUTO_ASSIGN_FOR_ATTACHMENT,
//                        new ValidationParameter("areaTypeCode", area.getAreaType().getCode()));
//            }
//            if (Boolean.TRUE.equals(attachByMedicalReason)) {
//                validation.error(AreaErrorReason.AREA_FLAGS_INCORRECT);
//            }
//        }
//        if (attachByMedicalReason != null && areaTypeAttachByMedicalReason != null &&
//                !Objects.equals(attachByMedicalReason, areaTypeAttachByMedicalReason)) {
//            validation.error(AreaErrorReason.ATTACH_BY_MEDICAL_REASON_INCORRECT,
//                    new ValidationParameter("attachByMedicalReason", attachByMedicalReason),
//                    new ValidationParameter("attachByMedicalReason", areaTypeAttachByMedicalReason));
//        }
        areaHelper.checkAreaTypeAgeSetups(area.getAreaType(), ageMin, ageMax, ageMinM, ageMaxM, ageMinW, ageMaxW, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
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

        if (areaHelper.isAreaPrimary(area)) {
            esuHelperService.sendAreaInfoEventTopicToESU(algorithms.createTopicAreaInfo(area, "updatePrimaryArea"));
        }

    }

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
    }

    @Override
    public Page<AddressAllocationOrders> searchOrder(Long id, String number, LocalDate date, String name, PageRequest paging) throws ContingentException {
        Validation validation = new Validation();

        if (id == null && number == null && date == null && name == null) {
            validation.error(AreaErrorReason.NO_SEARCH_PARAMETERS);
            throw new ContingentException(validation);
        }
        return addressAllocationOrderRepository.findAddressAllocationOrdersOverlapped(id, number, date, name, paging);
    }

    // (К_УУ_10)	Изменение медицинских работников на участке обслуживания
    @Override
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

        if (area.getAreaType().getCode() != AreaTypeKindEnum.MILDLY_ASSOCIATED.getCode()
                && area.getAreaType().getCode() != AreaTypeKindEnum.TREATMENT_ROOM_ASSOCIATED.getCode()) {

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
                        new ValidationParameter("startDate", startDate),
                        new ValidationParameter("endDate", endDate));
            }
        }
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        //5
        for (AddMedicalEmployee empl : addEmployeesInput) {
            //5.1
            if (empl.getStartDate().isBefore(LocalDate.now())) {
                validation.error(AreaErrorReason.START_DATE_IN_PAST,
                        new ValidationParameter("startDate", empl.getStartDate()));
            }

            // TODO в связи с изменениями в структуре БД блок требуется актуализировать
            //5.2
            Specialization specialization = specializationToPositionNomRepository.getSpecializationIdByPositionNomId(empl.getPositionId());

            //5.3
            if (specialization != null && !area.getAreaType().getSpecialization().getId().equals(specialization.getId())) {
                validation.error(AreaErrorReason.SPECIALIZATION_NOT_RELATED_TO_AREA,
                        new ValidationParameter("InputSpecialization", specialization.getTitle()),
                        new ValidationParameter("jobInfoId", empl.getMedicalEmployeeJobInfoId()),
                        new ValidationParameter("AreaSpecialization", area.getAreaType().getSpecialization().getTitle()));
            }

            //5.4
            List<AreaTypeMedicalPositions> positions = areaTypeMedicalPositionsRepository.getPositionsByAreaType(area.getAreaType().getCode());
            PositionNom inputPosition = positionNomCRUDRepository.findById(empl.getPositionId()).orElse(null);
            if (positions != null && positions.stream().anyMatch(pos -> pos.getPositionNom().getId() != empl.getPositionId())) {
                validation.error(AreaErrorReason.POSITION_NOT_SET_FOR_AREA_TYPE,
                        new ValidationParameter("positionTitle", inputPosition != null ? inputPosition.getTitle() : null),
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
        if (areaHelper.isAreaPrimary(area)) {
            esuHelperService.sendAreaInfoEventTopicToESU(algorithms.createTopicAreaInfo(area, "setMedicalEmployeeOnArea"));
        }

        return result;
    }

    // (К_УУ_12)	Получение подробной информации об участке
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
    @Override
    public List<Long> addAreaAddress(Long id, List<NsiAddress> nsiAddresses,
                                     List<NotNsiAddress> notNsiAddresses) throws ContingentException {
        Validation validation = new Validation();

        // 1. и 2.
        Area area = areaHelper.checkAndGetArea(id, validation);
        if (!validation.isSuccess()) { throw new ContingentException(validation); }

        // 3.
        areaHelper.noAddresses(nsiAddresses, notNsiAddresses);

        // 4.
        areaHelper.tooManyAddresses(nsiAddresses, notNsiAddresses, settingService.getPar1());

        // 5.
        areaAddressChecker.checkNsiAddresses(nsiAddresses, validation);

        // 6.
        areaAddressChecker.checkNotNsiAddresses(notNsiAddresses, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        // 7.
        Long serviceDistrictMO = algorithms.searchServiceDistrictMOByAddress(area.getMoId(), area.getAreaType(), null,
                nsiAddresses, notNsiAddresses, validation);

        // 8.
        areaHelper.checkAddressNotServiceByAreatype(area, nsiAddresses, notNsiAddresses, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        // 9.
        List<BuildingRegistry> buildingRegistries = new ArrayList<>();
        notNsiAddresses.forEach(nna -> {
            Optional<BuildingRegistry> buildingRegistryOptional = buildingRegistryRepository.findRegistryBuildings(
                    nna.getHouse(), nna.getBuilding(), nna.getConstruction(),
                    nna.getParentId()).stream().findFirst();
            if (!buildingRegistryOptional.isPresent()) {
                BuildingRegistry buildingRegistry = new BuildingRegistry(
                        nna.getParentId(), addressFormingElementRepository.findAfeByIdAndLevel(nna.getParentId(), nna.getLevelParentId()).get(0),
                        nna.getHouseType(), nna.getHouse(),
                        nna.getBuildingType(), nna.getBuilding(),
                        nna.getConstructionType(), nna.getConstruction());
                buildingRegistries.add(buildingRegistryCRUDRepository.save(buildingRegistry));
            } else {
                buildingRegistries.add(buildingRegistryOptional.get());
            }
        });

        // 10.
        List<Addresses> addresses = new ArrayList<>();
        for (NsiAddress nsiAddress: nsiAddresses) {
            AddressFormingElement addressFormingElement = null;
            BuildingRegistry buildingRegistry = null;

            if (nsiAddress.getLevelAddress() != 8) {
                addressFormingElement = addressFormingElementRepository.getAddressFormingElements(nsiAddress.getGlobalId(), nsiAddress.getLevelAddress()).get(0);
            } else {
                buildingRegistry = buildingRegistryRepository.getBuildingsRegistry(nsiAddress.getGlobalId()).get(0);
            }

            List<Addresses> foundAddresses = addressesRepository.findAddresses(nsiAddress.getLevelAddress(), buildingRegistry, addressFormingElement);

            if (!foundAddresses.isEmpty()) {
                addresses.add(foundAddresses.get(0));
            } else {
                Addresses address = new Addresses();
                address.setLevel(nsiAddress.getLevelAddress());
                address.setAddressFormingElement(addressFormingElement);
                address.setBuildingRegistry(buildingRegistry);
                addresses.add(addressesCRUDRepository.save(address));
            }

        }

        for (BuildingRegistry buildingRegistry: buildingRegistries) {
            List<Addresses> foundAddresses = addressesRepository.findAddresses(8L, buildingRegistry, null);

            if (!foundAddresses.isEmpty()) {
                addresses.add(foundAddresses.get(0));
            } else {
                Addresses address = new Addresses();
                address.setLevel(8);
                address.setBuildingRegistry(buildingRegistry);
                addresses.add(addressesCRUDRepository.save(address));
            }
        }

        // 11.
        List<Long> areaAddressIds = new ArrayList<>();

        addresses.forEach(address -> {
            AreaAddress areaAddress = new AreaAddress();
            areaAddress.setArea(area);
            areaAddress.setMoAddress(moAddressCRUDRepository.findById(serviceDistrictMO).orElse(null));
            areaAddress.setAddress(address);
            areaAddress.setCreateDate(LocalDateTime.now());
            areaAddress.setUpdateDate(LocalDateTime.now());
            areaAddressIds.add(areaAddressCRUDRepository.save(areaAddress).getId());
        });


        // 12.
        if (areaHelper.isAreaPrimary(area)) {
            esuHelperService.sendAreaInfoEventTopicToESU(algorithms.createTopicAreaInfo(area, "addAreaAddress"));
        }

        return areaAddressIds;
    }

    @Override
    public List<Long> addMoAddress(long moId, long areaTypeCode, long orderId, List<NsiAddress> nsiAddresses,
                                   List<NotNsiAddress> notNsiAddresses) throws ContingentException {
        Validation validation = new Validation();

        areaHelper.noAddresses(nsiAddresses, notNsiAddresses);

        areaHelper.tooManyAddresses(nsiAddresses, notNsiAddresses, settingService.getPar1());

        List<AreaType> areaTypes = areaHelper.checkAndGetAreaTypesExist(Collections.singletonList(areaTypeCode),
                validation, "areaTypeCode");

        AddressAllocationOrders order = addressAllocationOrderCRUDRepository.findById(orderId).orElse(null);

        if (order == null || Boolean.TRUE.equals(order.getArchived())) {
            throw new ContingentException(AreaErrorReason.ADDRESS_ALLOCATION_ORDER_NOT_EXISTS,
                    new ValidationParameter("orderId", orderId));
        }
        areaAddressChecker.checkNsiAddresses(nsiAddresses, validation);

        areaAddressChecker.checkNotNsiAddresses(notNsiAddresses, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        List<AddressWrapper> addresses = areaHelper.convertAddressToWrapper(nsiAddresses, notNsiAddresses);

        //Система для каждого переданного адреса выполняет поиск пересекающихся распределенных адресов
        areaAddressChecker.checkMoAddressesExist(moId, areaTypeCode, addresses, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
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
        //Система закрывает территории обслуживания участка, созданные в соответствии с архивируемой территорией обслуживания МО
        List<AreaAddress> areaAddresses = areaAddressRepository.findAreaAddresses(addresses.stream()
                .map(MoAddress::getId)
                .collect(Collectors.toList()));
        areaHelper.delAreaAddresses(areaAddresses);
        //Система закрывает территории обслуживания МО
        addresses.forEach(a -> {
            if (a.getStartDate().equals(LocalDate.now())) {
                moAddressCRUDRepository.delete(a);
            }
            else {
                a.setEndDate(LocalDate.now().minusDays(1));
                moAddressCRUDRepository.save(a);
            }
        });
    }


    // (К_УУ_10) Изменение участка обслуживания зависимого типа
    @Override
    public void updateDependentArea(long areaId, Long muId, List<MuType> muTypes, Integer muNumber,
                                    List<Long> primaryAreaTypeCodesAdd, List<Long> primaryAreaTypeCodesDel,
                                    Integer ageMin, Integer ageMax, Integer ageMinM, Integer ageMaxM, Integer ageMinW, Integer ageMaxW,
                                    boolean autoAssignForAttachment, String description) throws ContingentException {
        Validation validation = new Validation();
        //1 и 2
        Area area = areaHelper.checkAndGetArea(areaId, validation);
        Area oldArea = new Area(area);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        //3
        if (muId != null && !muId.equals(area.getMuId()) && area.getAreaType().getKindAreaType() != null
                && !Objects.equals(area.getAreaType().getKindAreaType().getCode(),
                AreaTypeKindEnum.TREATMENT_ROOM_ASSOCIATED.getCode())) {
            validation.error(AreaErrorReason.AREA_NOT_RELATED_TO_SPECIAL_OFFICE);
        }

        //4
        areaHelper.checkPrimaryAreaTypesForMuType(muTypes, primaryAreaTypeCodesAdd, validation);
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        //5
        areaHelper.checkAreaTypeAgeSetups(area.getAreaType(), ageMin, ageMax, ageMinM, ageMaxM, ageMinW, ageMaxW, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        //6 Обновление участка
        Long muIdFinal = muId == null ? area.getMuId() : muId;
        area.setMuId(muIdFinal);
        area.setNumber(muNumber == null ? area.getNumber() : muNumber);
        area.setAgeMax(ageMax == null ? area.getAgeMax() : ageMax);
        area.setAgeMin(ageMin == null ? area.getAgeMin() : ageMin);
        area.setAgeMMax(ageMaxM == null ? area.getAgeMMax() : ageMaxM);
        area.setAgeMMin(ageMinM == null ? area.getAgeMMin() : ageMinM);
        area.setAgeWMax(ageMaxW == null ? area.getAgeWMax() : ageMaxW);
        area.setAgeWMin(ageMinW == null ? area.getAgeWMin() : ageMinW);
        area.setAutoAssignForAttach(autoAssignForAttachment);
        area.setDescription(description == null ? area.getDescription() : description);
        area.setUpdateDate(LocalDateTime.now());

        areaHelper.resetAutoAssignForAttachment(area);
        //7 Обновление привязки к первичным типам участка
        List<AreaToAreaType> areaToAreaTypes = areaToAreaTypeRepository.getAreaTypesByAreaId(area.getId());

        List<AreaToAreaType> areaToAreaTypesToAdd = new ArrayList<>();

//        primaryAreaTypeCodesAdd.stream()
//                .filter(c -> areaToAreaTypes.stream().noneMatch(a -> Objects.equals(c, a.getAreaType().getCode())))
//                .forEach(c -> {
//            AreaToAreaType areaToAreaType = new AreaToAreaType();
//            areaToAreaType.setArea(area);
//            areaToAreaType.setAreaType(primaryAreaTypes.get(c));
//            areaToAreaTypeCRUDRepository.save(areaToAreaType);
//            areaToAreaTypesToAdd.add(areaToAreaType);
//        });
        List<AreaToAreaType> areaToAreaTypesToRemove = new ArrayList<>();

//                areaToAreaTypes.stream()
//                .filter(a -> a.getAreaType() != null &&
//                        !primaryAreaTypeCodesAdd.contains(a.getAreaType().getCode()) &&
//                        primaryAreaTypeCodesDel.contains(a.getAreaType().getCode()))
//                .collect(Collectors.toList());

        areaToAreaTypeCRUDRepository.deleteAll(areaToAreaTypesToRemove);

        //TODO падает при отправке в есу
        //esuService.saveAndPublishToESU(new AreaUpdateEvent(area, oldArea, areaToAreaTypesToAdd, areaToAreaTypesToRemove));
    }

    // (К_УУ_11) Удаление адресов из участка обслуживания
    @Override
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
        if (areaHelper.isAreaPrimary(area)) {
            esuHelperService.sendAreaInfoEventTopicToESU(algorithms.createTopicAreaInfo(area, "delAreaAddress"));
        }

        // 7.
        return;
    }

    // (К_УУ_12)	Получение списка адресов участка обслуживания
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

    // (К_УУ_13) Архивирование участка обслуживания
    @Override
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
        if (areaHelper.isAreaPrimary(area)) {
            esuHelperService.sendAreaInfoEventTopicToESU(algorithms.createTopicAreaInfo(area, "archiveArea"));
        }
    }

    // (К_УУ_14) Восстановление архивного участка обслуживания
    @Override
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
            // 7.1. Поиск зависимых участков
            // TODO логику фильтрации в репозиторий
            List<Area> areas = areaRepository.findAreas(area.getMuId() == null ? area.getMoId() : null, area.getMuId(),
                    (Long) null, null, true).stream()
                    .filter(a -> a.getPrimaryAreaTypes() != null &&
                            a.getPrimaryAreaTypes().stream()
                                    .anyMatch(t -> Objects.equals(t.getAreaType(), area.getAreaType())))
                    .collect(Collectors.toList());

            // 7.2.
            areas.forEach(depArea ->
                esuHelperService.sendAttachOnAreaChangeTopicToEsu(algorithms
                    .createTopicCreateCloseAttachAreaChange(Collections.singletonList(areaId), null, depArea))
                );
        }

        // 8.
        if (areaHelper.isAreaDependent(area)) {
            // TODO логику фильтрации в репозиторий
            List<Area> areas = areaRepository.findAreas(area.getMuId() == null ? area.getMoId() : null, area.getMuId(),
                    area.getPrimaryAreaTypes().stream()
                            .map(AreaToAreaType::getAreaType)
                            .map(AreaType::getCode)
                            .distinct()
                            .collect(Collectors.toList()),
                    null, true);

            esuHelperService.sendAttachOnAreaChangeTopicToEsu(algorithms
                    .createTopicCreateCloseAttachAreaChange(areas.stream().map(Area::getId).collect(Collectors.toList()),
                            null, area));
        }

        // 9.
        if (areaHelper.isAreaPrimary(area)) {
            esuHelperService.sendAreaInfoEventTopicToESU(algorithms.createTopicAreaInfo(area, "restoreArea"));
        }

        // 10.
        return;
    }

    // (К_УУ_20) Получение списка территорий обслуживания МО
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

    // (К_УУ_21) Получение идентификатора для создания нового участка
    @Override
    public Long getNewAreaId() throws ContingentException {
        return areaRepository.getNextAreaId();
    }

}
