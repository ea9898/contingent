package moscow.ptnl.contingent.area.service;

import moscow.ptnl.contingent.area.entity.area.AddressAllocationOrders;
import moscow.ptnl.contingent.area.entity.area.Area;
import moscow.ptnl.contingent.area.entity.area.AreaAddress;
import moscow.ptnl.contingent.area.entity.area.AreaMedicalEmployees;
import moscow.ptnl.contingent.area.entity.area.MoAddress;
import moscow.ptnl.contingent.area.entity.area.MoAvailableAreaTypes;
import moscow.ptnl.contingent.area.entity.area.MuAddlAreaTypes;
import moscow.ptnl.contingent.area.entity.area.MuAvailableAreaTypes;
import moscow.ptnl.contingent.area.entity.nsi.AreaPolicyTypes;
import moscow.ptnl.contingent.area.entity.nsi.AreaType;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeCountLimitEnum;
import moscow.ptnl.contingent.area.entity.nsi.PolicyType;
import moscow.ptnl.contingent.area.entity.nsi.PolicyTypeEnum;
import moscow.ptnl.contingent.area.error.AreaErrorReason;
import moscow.ptnl.contingent.area.error.ContingentException;
import moscow.ptnl.contingent.area.error.Validation;
import moscow.ptnl.contingent.area.error.ValidationParameter;
import moscow.ptnl.contingent.area.model.area.AddressLevelType;
import moscow.ptnl.contingent.area.model.area.AddressWrapper;
import moscow.ptnl.contingent.area.model.area.NotNsiAddress;
import moscow.ptnl.contingent.area.model.area.NsiAddress;
import moscow.ptnl.contingent.repository.area.AddressAllocationOrderCRUDRepository;
import moscow.ptnl.contingent.repository.area.AreaAddressCRUDRepository;
import moscow.ptnl.contingent.repository.area.AreaCRUDRepository;
import moscow.ptnl.contingent.repository.area.AreaMedicalEmployeeCRUDRepository;
import moscow.ptnl.contingent.repository.area.AreaRepository;
import moscow.ptnl.contingent.repository.area.MoAddressCRUDRepository;
import moscow.ptnl.contingent.repository.area.MoAvailableAreaTypesRepository;
import moscow.ptnl.contingent.repository.area.MuAddlAreaTypesRepository;
import moscow.ptnl.contingent.repository.area.MuAvailableAreaTypesRepository;
import moscow.ptnl.contingent.repository.nsi.AddressFormingElementRepository;
import moscow.ptnl.contingent.repository.nsi.AreaPolicyTypesCRUDRepository;
import moscow.ptnl.contingent.repository.nsi.AreaPolicyTypesRepository;
import moscow.ptnl.contingent.repository.nsi.AreaTypesCRUDRepository;
import moscow.ptnl.contingent.repository.nsi.BuildingRegistryRepository;
import moscow.ptnl.contingent.repository.nsi.PositionNomRepository;
import moscow.ptnl.contingent.area.transform.NotNsiAddressMapper;
import moscow.ptnl.contingent.area.transform.NsiAddressMapper;
import moscow.ptnl.contingent.area.util.Period;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Component;
import ru.mos.emias.contingent2.core.AddMedicalEmployee;
import ru.mos.emias.contingent2.core.ChangeMedicalEmployee;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.Month;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

@Component
public class AreaServiceHelper {

    public static final Integer NOT_NSI_ADDRESS_LEVEL = 8;
    private static final Long PRIMARY_AREA_TYPE_CLASS = 1L;
    private static final Long DEPENDENT_AREA_TYPE_CLASS = 2L;

    @Autowired
    private AreaTypesCRUDRepository areaTypesCRUDRepository;

    @Autowired
    private MuAddlAreaTypesRepository muAddlAreaTypesRepository;

    @Autowired
    private AreaRepository areaRepository;

    @Autowired
    private AreaCRUDRepository areaCRUDRepository;

    @Autowired
    private AddressAllocationOrderCRUDRepository addressAllocationOrderCRUDRepository;

    @Autowired
    private MoAddressCRUDRepository moAddressCRUDRepository;

    @Autowired
    private PositionNomRepository positionNomRepository;

    @Autowired
    private AreaAddressCRUDRepository areaAddressCRUDRepository;

    @Autowired
    private AreaMedicalEmployeeCRUDRepository areaMedicalEmployeeCRUDRepository;

    @Autowired
    private BuildingRegistryRepository buildingRegistryRepository;

    @Autowired
    private SettingService settingService;

    @Autowired
    NsiAddressMapper nsiAddressMapper;

    @Autowired
    NotNsiAddressMapper notNsiAddressMapper;

    @Autowired
    private AddressFormingElementRepository addressFormingElementRepository;

    @Autowired
    private MoAvailableAreaTypesRepository moAvailableAreaTypesRepository;

    @Autowired
    private MuAvailableAreaTypesRepository muAvailableAreaTypesRepository;

    @Autowired
    private Algorithms algorithms;

    @Autowired
    private AreaPolicyTypesRepository areaPolicyTypesRepository;

    @Autowired
    private AreaPolicyTypesCRUDRepository areaPolicyTypesCRUDRepository;

    /* Система проверяет, что в справочнике «Типы участков» (AREA_TYPES) существует каждый входной параметр
    «ИД типа участка» с признаком архивности = 0.
    Иначе возвращает ошибку */
    public List<AreaType> checkAndGetAreaTypesExist(List<Long> areaTypes, Validation validation, String parameterCode) {
        List<AreaType> result = new ArrayList<>();

        areaTypes.forEach(a -> {
            Optional<AreaType> areaType = areaTypesCRUDRepository.findById(a);

            if (!areaType.isPresent() || Boolean.TRUE.equals(areaType.get().getArchived())) {
                validation.error(AreaErrorReason.AREA_TYPE_NOT_FOUND, new ValidationParameter(parameterCode, a));
            }
            else {
                result.add(areaType.get());
            }
        });
        return result;
    }

    /* Система проверяет, что в базе данных нет записи в таблице «Профиль МУ» (PROFILE_MU)
    со значениями из входных параметров:
   •	ИД МУ (MU_ID) = input ИД МУ;
   •	ИД типа участка (AREA_TYPE_CODE) = input ИД типа участка.
   Иначе возвращает ошибку */
    public void checkMuAddlAreaTypeExist(Long muId, List<Long> areaTypes, Validation validation) {
        List<MuAddlAreaTypes> muAddlAreaTypes = muAddlAreaTypesRepository.findMuAddlAreaTypes(
                Collections.singletonList(muId), areaTypes);

        if (muAddlAreaTypes != null && !muAddlAreaTypes.isEmpty()) {
            for (MuAddlAreaTypes muAddlAreaType : muAddlAreaTypes) {
                validation.error(AreaErrorReason.AREA_TYPE_ALREADY_EXISTS,
                        new ValidationParameter("areaType", muAddlAreaType.getAreaType().getTitle()));
            }
        }
    }

    /* Система проверяет наличие в профиле МУ переданных типов участка. */
    public void checkMuProfilesHasAreaTypes(Long muId, List<Long> areaTypes, Validation validation) {
        List<MuAddlAreaTypes> muAddlAreaTypes = muAddlAreaTypesRepository.getMuAddlAreaTypes(muId);

        List<Long> areaTypesProfiles = muAddlAreaTypes.stream().map(MuAddlAreaTypes::getAreaType).map(AreaType::getCode).collect(Collectors.toList());
        List<Long> areaTypesDiff =
                areaTypes.stream().filter(areaType -> !areaTypesProfiles.contains(areaType))
                .collect(Collectors.toList());

        if (!areaTypesDiff.isEmpty()) {
            validation.error(AreaErrorReason.AREA_TYPE_NOT_EXISTS_IN_MO,
                new ValidationParameter("areaType", areaTypesDiff.stream().map(String::valueOf).collect(Collectors.joining(", "))));
        }
    }

    /* Система проверяет, что у данной МУ отсутствуют активные участки данного типа (AREAS):
    •	ИД МУ (MU_ID) = input ИД МУ;
    •	ИД типа участка (AREA_TYPE_CODE) = input ИД тип участка;
    •	Архивность (ARCHIVE) = 0.
     */
    public void checkMuActiveAreasNotExist(Long muId, List<Long> areaTypes, Validation validation) {
        List<Area> areas = areaRepository.findAreas(null, muId, areaTypes, null,true);

        if (!areas.isEmpty()) {
            for (Area area: areas) {
                validation.error(AreaErrorReason.CANT_DELETE_AREA_TYPE,
                        new ValidationParameter("areaType", area.getAreaType().getTitle()),
                        new ValidationParameter("areaNumber", area.getNumber()));
            }

        }
    }

    public void checkAreaTypeAgeSetups(AreaType areaType, Integer ageMin, Integer ageMax,
                                       Integer ageMinM, Integer ageMaxM, Integer ageMinW, Integer ageMaxW, Validation validation) {
        if (checkAgeSetupFilling(ageMin, ageMax, areaType.getAgeMin(), areaType.getAgeMax()) ||
                checkAgeSetupFilling(ageMinM, ageMaxM, areaType.getAgeMMin(), areaType.getAgeMMax()) ||
                checkAgeSetupFilling(ageMinW, ageMaxW, areaType.getAgeWMin(), areaType.getAgeWMax())) {
            validation.error(AreaErrorReason.INCORRECT_AREA_AGE_SETUPS);
            return;
        }
        checkAgeSetupRange(ageMin, ageMax, areaType.getAgeMin(), areaType.getAgeMax(), "ageMin", "ageMax", validation);
        checkAgeSetupRange(ageMinM, ageMaxM, areaType.getAgeMMin(), areaType.getAgeMMax(), "ageMinM", "ageMaxM", validation);
        checkAgeSetupRange(ageMinW, ageMaxW, areaType.getAgeWMin(), areaType.getAgeWMax(), "ageMinW", "ageMaxW", validation);
    }

    private boolean checkAgeSetupFilling(Integer ageMin, Integer ageMax, Integer ageMinAreaType, Integer ageMaxAreaType) {
        return (ageMin == null && ageMinAreaType != null) || (ageMax == null && ageMaxAreaType != null);
    }

    public void checkAgeSetupRange(Integer ageMin, Integer ageMax, Integer ageMinAreaType, Integer ageMaxAreaType,
                                       String paramMinCode, String paramMaxCode, Validation validation) {
        if (!(ageMin == null || ageMin >= ageMinAreaType) && !(ageMax == null || ageMax <= ageMaxAreaType)) {
            validation.error(AreaErrorReason.AREA_AGE_SETUP_EXCEEDED,
                    new ValidationParameter(paramMinCode, ageMin), new ValidationParameter(paramMaxCode, ageMax),
                    new ValidationParameter(paramMinCode, ageMinAreaType), new ValidationParameter(paramMaxCode, ageMaxAreaType));
        }
    }

    // К_УУ_8 2.
    // Система проверяет наличие каждого переданного первичного типа участка
    public void checkPrimaryAreasTypesInMUProfile(Long muId, AreaType areaType, Validation validation) {
        List<MuAvailableAreaTypes> muAvailableAreaTypes = new ArrayList<>();
        List<MoAvailableAreaTypes> moAvailableAreaTypes = new ArrayList<>();

        if (muId != null) {
            muAvailableAreaTypes = muAvailableAreaTypesRepository.findByAreaTypes(areaType);
        } else {
            moAvailableAreaTypes = moAvailableAreaTypesRepository.findByAreaTypes(areaType);
        }

        if (moAvailableAreaTypes.size() + muAvailableAreaTypes.size() == 0) {
            validation.error(AreaErrorReason.AREA_TYPE_NOT_AVAILABLE_FOR_MU,
                    new ValidationParameter("areaType", areaType.getCode()));
        }
    }

    // Проверяет существует ли участок с указанным идентификатором и не находится ли он в архиве
    public Area checkAndGetArea(long areaId, Validation validation) {
        Area area = areaCRUDRepository.findById(areaId).orElse(null);

        // Система выполняет поиск участка, по входному параметру ИД участка в БД.
        // Участок найден, иначе возвращает ошибку
        if (area == null) {
            validation.error(AreaErrorReason.AREA_NOT_FOUND, new ValidationParameter("areaId", areaId));
        }
        else
        // Система проверяет, что участок не находится в архиве, иначе возвращает ошибку
            if (area.getArchived()) {
            validation.error(AreaErrorReason.AREA_IS_ARCHIVED, new ValidationParameter("areaId", areaId));
        }
        return area;
    }

    public Area checkAndGetArchivedArea(long areaId, Validation validation) {
        Area area = areaCRUDRepository.findById(areaId).orElse(null);

        if (area == null) {
            validation.error(AreaErrorReason.AREA_NOT_FOUND, new ValidationParameter("areaId", areaId));
        }
        else if (!area.getArchived()) {
            validation.error(AreaErrorReason.AREA_IS_NOT_ARCHIVED, new ValidationParameter("areaId", areaId));
        }
        return area;
    }

    public void checkDateTillToday(LocalDate date, Validation validation) {
        if (date.isBefore(LocalDate.of(1970, Month.JANUARY, 1)) ||
                date.isAfter(LocalDate.now())) {
            validation.error(AreaErrorReason.DATE_IN_INCORRECT_PERIOD);
        }
    }

    /*
    Система проверяет, что вид участка отличен от «Именной»
     */
    public void checkAreaTypeIsNotPersonal(AreaType areaType, Validation validation) {
        if (areaType.getAreaTypeKind() != null &&
                Objects.equals(areaType.getAreaTypeKind().getCode(), 4L)) {
            validation.error(AreaErrorReason.CANT_RESTORE_PERSONAL_KIND_AREA);
        }
    }

    //Todo т.к. МУ ИД теперь не обязательный, видимо нужно учитывать МО ИД ?
    public void checkAreaExistsInMU(long muId, long areaTypeCode, Integer number, Long excludeAreaId, Validation validation) {
        List<Area> areas = areaRepository.findAreas(null, muId, areaTypeCode, number, true);

        if (areas.stream().anyMatch(a -> excludeAreaId == null || !Objects.equals(a.getId(), excludeAreaId))) {
            validation.error(AreaErrorReason.AREA_WITH_TYPE_AND_NUMBER_EXISTS_IN_MO,
                    new ValidationParameter("areaTypeCode", areaTypeCode),
                    new ValidationParameter("number", number));
        }
    }

    public void checkOrderExists(long orderId, Validation validation) {
        Optional<AddressAllocationOrders> order = addressAllocationOrderCRUDRepository.findById(orderId);

        if (!order.isPresent() || Boolean.TRUE.equals(order.get().getArchived())) {
            validation.error(AreaErrorReason.ADDRESS_ALLOCATION_ORDER_NOT_EXISTS, new ValidationParameter("orderId", orderId));
        }
    }

    public void checkNoAddresses(List<NsiAddress> nsiAddresses,
                                 List<NotNsiAddress> notNsiAddresses) throws ContingentException {
        if (nsiAddresses.size() + notNsiAddresses.size() == 0) {
            throw new ContingentException(AreaErrorReason.NO_ADDRESS);
        }
    }

    public void checkTooManyAddresses(List<NsiAddress> nsiAddresses,
                                      List<NotNsiAddress> notNsiAddresses, Long maxAddresses) throws ContingentException {
        if (nsiAddresses.size() + notNsiAddresses.size() > maxAddresses) {
            throw new ContingentException(AreaErrorReason.TOO_MANY_ADDRESSES, new ValidationParameter("maxAddresses", maxAddresses));
        }
    }

    public List<MoAddress> getAndCheckMoAddressesExist(List<Long> moAddressIds, Validation validation) {
        List<MoAddress> result = new ArrayList<>();

        moAddressIds.forEach(a -> {
            Optional<MoAddress> order = moAddressCRUDRepository.findById(a);

            if (!order.isPresent() ||
                    order.get().getEndDate() != null && order.get().getEndDate().isBefore(LocalDate.now())) {
                validation.error(AreaErrorReason.MO_ADDRESS_NOT_EXISTS, new ValidationParameter("moAddressId", a));
            }
            else {
                result.add(order.get());
            }
        });
        return result;
    }

    public void checkMainEmployeesOverlappingDates(List<AreaMedicalEmployees> mainEmployees,
                                                   Validation validation) throws ContingentException {
        if (mainEmployees.size() > 1) {
            mainEmployees.sort(Comparator.comparing(AreaMedicalEmployees::getStartDate));
            for (int i = 0; i < mainEmployees.size() - 1; i++) {
                AreaMedicalEmployees current = mainEmployees.get(i);
                AreaMedicalEmployees next = mainEmployees.get(i + 1);
                if (current.getEndDate() == null
                        || next.getStartDate().minusDays(1).isBefore(current.getEndDate())) {
                    validation.error(AreaErrorReason.MAIN_EMPLOYEE_DATE_OVERLAP,
                            new ValidationParameter("specialization1",
                                    current.getMedicalEmployeeJobInfoId()),
                            new ValidationParameter("specialization2",
                                    next.getMedicalEmployeeJobInfoId()));
                }
            }
            if (!validation.isSuccess()) {
                throw new ContingentException(validation);
            }
        }
    }

    public List<AddressWrapper> convertAddressToWrapper(List<NsiAddress> nsiAddresses,
                                                        List<NotNsiAddress> notNsiAddresses) {
        //Сформируем список адресов по справочнику
        List<AddressWrapper> addresses = nsiAddresses.stream()
                .map(AddressWrapper::new)
                .peek(a -> {
                    if (a.nsiAddress.getLevelAddress() != AddressLevelType.ID.getLevel()) {
                        a.addressFormingElement = addressFormingElementRepository.getAddressFormingElements(
                                a.nsiAddress.getGlobalId(), a.nsiAddress.getLevelAddress()).get(0);
                    }
                    else {
                        a.buildingRegistry = buildingRegistryRepository.getBuildingsRegistry(a.nsiAddress.getGlobalId()).get(0);
                    }
                })
                .collect(Collectors.toList());

        //Сформируем список адресов вне справочника
        addresses.addAll(notNsiAddresses.stream()
                .map(notNsi -> {
                    AddressWrapper addressWrapper = new AddressWrapper();
                    addressWrapper.setNotNsiAddress(notNsi);
                    addressWrapper.setAddressFormingElement(addressFormingElementRepository.getAddressFormingElements(notNsi.getParentId(), notNsi.getLevelParentId()).get(0));
                    return addressWrapper;
                }).collect(Collectors.toList()));

        return addresses;
    }


    public void checkReplacementWithoutMain(List<Period> periodsWithoutMainEmpl,
                                            List<AreaMedicalEmployees> replacementEmployees,
                                            Validation validation) throws ContingentException {
        boolean foundError = false;
        for (Period period : periodsWithoutMainEmpl) {
            for (AreaMedicalEmployees empl : replacementEmployees) {
                if (period.isInterceptWith(empl.getStartDate(), empl.getEndDate())) {
                    foundError = true;
                    break;
                }
            }
            if (foundError) {
                validation.error(AreaErrorReason.REPLACEMENT_WITHOUT_MAIN_EMPLOYEE,
                        new ValidationParameter("startDate", period.getStartDate()),
                        new ValidationParameter("endDate",period.getEndDate()));
                foundError = false;
            }
        }
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
    }

    public void checkDatesNotInterceptWithSamePosition(List<AreaMedicalEmployees> allEmployees,
                                                       Validation validation) throws ContingentException {
        if (allEmployees.size() > 1) {
            allEmployees.sort(Comparator.comparing(AreaMedicalEmployees::getMedicalEmployeeJobInfoId)
                    .thenComparing(AreaMedicalEmployees::getStartDate));
            for (int i = 0; i < allEmployees.size() - 1; i++) {
                AreaMedicalEmployees current = allEmployees.get(i);
                AreaMedicalEmployees next = allEmployees.get(i + 1);
                if (current.getMedicalEmployeeJobInfoId() != null
                        && current.getMedicalEmployeeJobInfoId().equals(next.getMedicalEmployeeJobInfoId())
                        && (current.getEndDate() == null || next.getStartDate().isBefore(current.getEndDate().plusDays(1)))) {
                    validation.error(AreaErrorReason.JOB_ID_DATE_OVERLAP,
                            new ValidationParameter("jobInfoId", current.getMedicalEmployeeJobInfoId()),
                            new ValidationParameter("areaId", current.getArea().getId()),
                            new ValidationParameter("startDate1", current.getStartDate()),
                            new ValidationParameter("endDate1",
                                    current.getEndDate() != null ? current.getEndDate() : Period.MAX_DATE),
                            new ValidationParameter("startDate2", next.getStartDate()),
                            new ValidationParameter("endDate2",
                                    next.getEndDate() != null ? next.getEndDate() : Period.MAX_DATE));
                }
            }
            if (!validation.isSuccess()) {
                throw new ContingentException(validation);
            }
        }
    }

    public List<Period> getPeriodsWithoutMainEmployee(List<AreaMedicalEmployees> mainEmployees) {
        mainEmployees.sort(Comparator.comparing(AreaMedicalEmployees::getStartDate));
        List<Period> periodsWithoutMainEmpl = new ArrayList<>();
        if (mainEmployees.isEmpty()) {
            periodsWithoutMainEmpl.add(Period.ALL_TIME);
            return periodsWithoutMainEmpl;
        }
        AreaMedicalEmployees first = mainEmployees.get(0);
        periodsWithoutMainEmpl.add(new Period(Period.MIN_DATE, first.getStartDate().minusDays(1)));
        for (int i = 0; i < mainEmployees.size() - 1; i++) {
            AreaMedicalEmployees current = mainEmployees.get(i);
            AreaMedicalEmployees next = mainEmployees.get(i + 1);
            if (current.getEndDate() == null) {
                return periodsWithoutMainEmpl;
            }
            if (next.getStartDate().minusDays(1).isAfter(current.getEndDate())) {
                periodsWithoutMainEmpl.add(new Period(current.getEndDate().plusDays(1), next.getStartDate().minusDays(1)));
            }
        }
        AreaMedicalEmployees last = mainEmployees.get(mainEmployees.size() - 1);
        if (last.getEndDate() != null) {
            periodsWithoutMainEmpl.add(new Period(last.getEndDate().plusDays(1), Period.MAX_DATE));
        }
        return periodsWithoutMainEmpl;
    }

    public void applyChanges(List<AreaMedicalEmployees> employees, List<ChangeMedicalEmployee> changeEmployees) {
        for (AreaMedicalEmployees empl : employees) {
            Optional<ChangeMedicalEmployee> optionalChangeEmpl = changeEmployees.stream().filter(
                    ch -> empl.getId().equals(ch.getAssignmentId())).findFirst();
            if (!optionalChangeEmpl.isPresent()) {
                continue;
            }
            ChangeMedicalEmployee changeEmpl = optionalChangeEmpl.get();
            if (changeEmpl.getStartDate() != null) {
                empl.setStartDate(changeEmpl.getStartDate());
            }

            if (changeEmpl.getEndDate() != null) {
                empl.setEndDate(changeEmpl.getEndDate());
            }
        }
    }

    public void addNew(List<AreaMedicalEmployees> employees, List<AddMedicalEmployee> addEmployees, Area area) {
        addEmployees.forEach(empl -> employees.add(new AreaMedicalEmployees(
                empl.getMedicalEmployeeJobInfoId(),
                area,
                empl.isIsReplacement(),
                empl.getStartDate(),
                empl.getEndDate(),
                empl.getSnils(),
                positionNomRepository.getPositionProxy(empl.getPositionId()),
                LocalDateTime.now(),
                LocalDateTime.now(),
                empl.getSubdivisionId())));
    }


    public void delAreaAddresses(List<AreaAddress> addresses) {
        addresses.forEach(a -> {
            if (a.getStartDate().equals(LocalDate.now())) {
                areaAddressCRUDRepository.delete(a);
            }
            else {
                a.setEndDate(LocalDate.now().minusDays(1));
            }
        });
    }

    public void delAreaMedicalEmployees(List<AreaMedicalEmployees> employees) {
        employees.forEach(a -> {
            if (a.getStartDate().equals(LocalDate.now())) {
                areaMedicalEmployeeCRUDRepository.delete(a);
            }
            else {
                a.setEndDate(LocalDate.now().minusDays(1));
            }
        });
    }

    public boolean isAreaPrimary(Area area) {
        return area.getAreaType() != null && area.getAreaType().getAreaTypeClass() != null &&
                Objects.equals(PRIMARY_AREA_TYPE_CLASS, area.getAreaType().getAreaTypeClass().getCode());
    }

    public boolean isAreaDependent(Area area) {
        return area.getAreaType() != null && area.getAreaType().getAreaTypeClass() != null &&
                Objects.equals(DEPENDENT_AREA_TYPE_CLASS, area.getAreaType().getAreaTypeClass().getCode());
    }

    // К_УУ_11 3.
    // Система проверяет, что количество территорий обслуживания не превышает максимально
    // допустимое значение, указанное во внутреннем системном параметре (PAR_2).
    // Иначе возвращает ошибку
    public void tooManyAreaAddresses(List<Long> areaAddressIds, Long maxAreaAddress, Validation validation) {
        if (areaAddressIds.size() > maxAreaAddress) {
            validation.error(AreaErrorReason.TOO_MANY_ADDRESSES, new ValidationParameter("maxAddresses", maxAreaAddress));
        }
    }

    // Система проверяет, что размер страницы, указанный во входных параметрах, не превышает максимально допустимое значение, указанное во внутреннем системном параметре (PAR_3).
    // Иначе возвращает ошибку
    public void checkMaxPage(PageRequest paging, Validation validation) {
        if (paging != null && paging.getPageSize() > settingService.getPar3()) {
            validation.error(AreaErrorReason.TOO_BIG_PAGE_SIZE, new ValidationParameter("pageSize", settingService.getPar3()));
        }
    }

    public void resetAutoAssignForAttachment(Area area) {
        if (area.getAutoAssignForAttach()) {
            List<Area> areas = areaRepository.findAreas(null, area.getMuId(), area.getAreaType().getCode(), null, null);
            areas.stream().filter(a -> !Objects.equals(area.getId(), a.getId())).forEach(a -> a.setAutoAssignForAttach(false));
        }
    }

    private List<ValidationParameter> getValidationParamsForNotNsiAddress(NotNsiAddress notNsiAddress) {
        List<ValidationParameter> validationParameters = new ArrayList<>();
        validationParameters.add(new ValidationParameter("id", notNsiAddress.getParentId()));
        validationParameters.add(new ValidationParameter("house", notNsiAddress.getHouseType() + notNsiAddress.getHouse()));
        if (notNsiAddress.getConstructionType() != null && notNsiAddress.getConstruction() != null) {
            validationParameters.add(new ValidationParameter("construction", notNsiAddress.getConstructionType() + notNsiAddress.getConstruction()));
        } else {
            validationParameters.add(new ValidationParameter("construction", ""));
        }
        if (notNsiAddress.getBuildingType() != null && notNsiAddress.getBuilding() != null) {
            validationParameters.add(new ValidationParameter("building", notNsiAddress.getBuildingType() + notNsiAddress.getBuilding()));
        } else {
            validationParameters.add(new ValidationParameter("building", ""));
        }
        return validationParameters;
    }

    /* К_УУ_13 Система проверяет, что каждый из списка адресов не обслуживается участком такого же типа, как и участок из входных параметров */
    public void checkAddressNotServiceByAreatype(Area area, List<AddressWrapper> addressWrapperList, Validation validation) throws ContingentException {
        for (AddressWrapper addressWrapper: addressWrapperList) {
            if (addressWrapper.getNsiAddress() != null) {
                NsiAddress nsiAddress = addressWrapper.getNsiAddress();
                Long foundAreaId = algorithms.searchAreaByAddress(area.getMoId(), area.getAreaType(),
                        Collections.singletonList(nsiAddress), new ArrayList<>());
                if (foundAreaId != null) {
                    if (!foundAreaId.equals(area.getId())) {
                        validation.error(AreaErrorReason.ADDRESS_ALREADY_SERVICED_ANOTHER_AREA,
                                new ValidationParameter("areaId", foundAreaId));
                    } else {
                        validation.error(AreaErrorReason.ADDRESS_ALREADY_SERVICED_NSI,
                                new ValidationParameter("id", nsiAddress.getGlobalId()),
                                new ValidationParameter("level", nsiAddress.getLevelAddress()));
                    }
                }
            } else {
                NotNsiAddress notNsiAddress = addressWrapper.getNotNsiAddress();
                Long foundAreaId = algorithms.searchAreaByAddress(area.getMoId(), area.getAreaType(),
                        new ArrayList<>(), Collections.singletonList(notNsiAddress));
                if (foundAreaId != null) {
                    if (!foundAreaId.equals(area.getId())) {
                        validation.error(AreaErrorReason.ADDRESS_ALREADY_SERVICED_ANOTHER_AREA,
                                new ValidationParameter("areaId", foundAreaId));
                    } else {
                        List<ValidationParameter> validationParameters = getValidationParamsForNotNsiAddress(notNsiAddress);
                        validation.error(AreaErrorReason.ADDRESS_ALREADY_SERVICED_NOTNSI,
                                validationParameters.toArray(new ValidationParameter[validationParameters.size()]));
                    }
                }
            }
        }
    }

    public List<AddressWrapper> convertToAddressWarapper(List<NsiAddress> nsiAddresses, List<NotNsiAddress> notNsiAddresses) {
        List<AddressWrapper> addressWrapperList = new ArrayList<>();
        if (nsiAddresses != null) {
            addressWrapperList.addAll(nsiAddresses.stream().map(AddressWrapper::new).collect(Collectors.toList()));
        }
        if (notNsiAddresses != null) {
            addressWrapperList.addAll(notNsiAddresses.stream().map(AddressWrapper::new).collect(Collectors.toList()));
        }
        return addressWrapperList;
    }

    public void addMoAddressToAddressWrapper(Area area, List<AddressWrapper> addressWrapperList, Validation validation) throws ContingentException {

        for (AddressWrapper addressWrapper: addressWrapperList) {
            MoAddress serviceDistrictMO = algorithms.searchServiceDistrictMOByAddress(area.getMoId(), area.getAreaType(), null,
                    addressWrapper.getNsiAddress() != null ? Collections.singletonList(addressWrapper.getNsiAddress()) : new ArrayList<>(),
                    addressWrapper.getNotNsiAddress() != null ? Collections.singletonList(addressWrapper.getNotNsiAddress()) : new ArrayList<>(),
                    validation);

            if (serviceDistrictMO != null && serviceDistrictMO.getMoId().equals(area.getMoId())) {
                // если найдена территории обслуживания
                addressWrapper.setMoAddress(serviceDistrictMO);
            } else if (serviceDistrictMO == null) {
                // если территория обслуживания МО для адреса не найдена
                validation.error(AreaErrorReason.INCORRECT_ADDRESS_NESTING);
            } else {
                // найдена территория обслуживания, где ИД МО <> ИД МО участка
                if (addressWrapper.getNsiAddress() != null) {
                    // НСИ адрес
                    validation.error(
                            AreaErrorReason.ADDRESS_NOT_SERVICED_MO__NSI,
                            new ValidationParameter("levelAddress", addressWrapper.getNsiAddress().getLevelAddress()),
                            new ValidationParameter("globalId", addressWrapper.getNsiAddress().getGlobalId()),
                            new ValidationParameter("moId", area.getMoId()));
                } else {
                    // НеНСИ адрес
                    NotNsiAddress notNsiAddress = addressWrapper.getNotNsiAddress();
                    List<ValidationParameter> validationParameters = getValidationParamsForNotNsiAddress(notNsiAddress);
                    validationParameters.add(new ValidationParameter("moId", area.getMoId()));
                    validation.error(AreaErrorReason.ADDRESS_NOT_SERVICED_MO_NOTNSI, validationParameters.toArray(new ValidationParameter[validationParameters.size()]));
                }
            }
        }
    }

    // К_УУ_1 2.
    // Система проверяет, что в списке доступных для МО отсутствует Тип участка с переданным кодом
    public void checkAreaTypesExistInMO(long moId, List<AreaType> areaTypes, Validation validation) {
        List<AreaType> availableAreaTypes = moAvailableAreaTypesRepository.findAreaTypes(moId).stream()
                .map(MoAvailableAreaTypes::getAreaType)
                .collect(Collectors.toList());
        areaTypes.forEach(a -> {
            if (availableAreaTypes.contains(a)) {
                validation.error(AreaErrorReason.AREA_TYPE_ALREADY_EXISTS,
                        new ValidationParameter("areaType", a.getTitle()));
            }
        });
    }

    // К_УУ_4 2.
    // Система проверяет, что в списке доступных для МУ отсутствует Тип участка с переданным кодом
    public void checkAreaTypesExistInMU(long muId, List<AreaType> areaTypes, Validation validation) {
        List<AreaType> availableAreaTypes = muAvailableAreaTypesRepository.findAreaTypes(muId).stream()
                .map(MuAvailableAreaTypes::getAreaType)
                .collect(Collectors.toList());
        areaTypes.forEach(a -> {
            if (availableAreaTypes.contains(a)) {
                validation.error(AreaErrorReason.AREA_TYPE_ALREADY_EXISTS,
                        new ValidationParameter("areaType", a.getTitle()));
            }
        });
    }

    // К_УУ_2 1., К_УУ_4 1.
    // Система проверяет, что в списке доступных для МО присутствует Тип участка с переданным кодом
    public List<MoAvailableAreaTypes> checkAndGetAreaTypesNotExistInMO(long moId, List<Long> areaTypeCodes, Validation validation) {
        List<MoAvailableAreaTypes> moAvailableAreaTypes = moAvailableAreaTypesRepository.findAreaTypes(moId);
        List<Long> availableAreaTypes = moAvailableAreaTypes.stream()
                .map(MoAvailableAreaTypes::getAreaType)
                .map(AreaType::getCode)
                .collect(Collectors.toList());
        areaTypeCodes.forEach(a -> {
            if (!availableAreaTypes.contains(a)) {
                validation.error(AreaErrorReason.AREA_TYPE_NOT_EXISTS_IN_MO,
                        new ValidationParameter("areaType", areaTypesCRUDRepository.findById(a).map(AreaType::getTitle).orElse(String.valueOf(a))));
            }
        });
        return moAvailableAreaTypes.stream()
                .filter(a -> areaTypeCodes.contains(a.getAreaType().getCode()))
                .collect(Collectors.toList());
    }

    // К_УУ_2 2.
    // Система проверяет, что тип участка не присутствует ни в одном списке доступных для МУ
    public void checkAndGetAreaTypesNotExistInMU(List<MoAvailableAreaTypes> moAvailableAreaTypes, List<Long> areaTypeCodes, Validation validation) {
        Map<AreaType, List<MuAvailableAreaTypes>> found = moAvailableAreaTypes.stream()
                .filter(a -> a.getMuAvailableAreaTypes() != null)
                .flatMap(a -> a.getMuAvailableAreaTypes().stream())
                .filter(a -> areaTypeCodes.contains(a.getAreaType().getCode()))
                .collect(Collectors.groupingBy(MuAvailableAreaTypes::getAreaType));

        found.forEach((t, a) -> validation.error(AreaErrorReason.CANT_DELETE_AREA_TYPE,
                new ValidationParameter("areaTypeCode", t.getCode()),
                new ValidationParameter("muId", a.stream()
                        .map(MuAvailableAreaTypes::getMuId)
                        .map(String::valueOf)
                        .distinct()
                        .collect(Collectors.joining(", ")))
                )
        );
    }

    public void checkPolicyTypesIsOMS(List<Long> policyTypesAdd,  Validation validation) throws ContingentException {
        if (!policyTypesAdd.isEmpty() && policyTypesAdd.stream().anyMatch(policy -> !policy.equals(PolicyTypeEnum.OMS.getCode()))) {
            validation.error(AreaErrorReason.NO_PRIMARY_AREA);
            throw new ContingentException(validation);
        }
    }

    public void checkPolicyTypesDel(Area area, List<PolicyType> policyTypesDel, Validation validation) throws ContingentException {
        if (!policyTypesDel.isEmpty()) {
            for (PolicyType policy : policyTypesDel) {
                if (areaPolicyTypesRepository.findAll(area, policy).isEmpty()) {
                    validation.error(AreaErrorReason.POLICY_TYPE_NOT_SET_FOR_AREA,
                            new ValidationParameter("areaTypeCode", policy),
                            new ValidationParameter("areaid", area.getId()));
                }
            }
            if (!validation.isSuccess()) {
                throw new ContingentException(validation);
            }
        }
    }

    public void saveAndDeleteAreaPolicyTypes(Area area, List<PolicyType> policyTypesAdd, List<PolicyType> policyTypesDel) throws ContingentException {

        if (!policyTypesDel.isEmpty()) {
            areaPolicyTypesRepository.deleteAll(area, policyTypesDel);
        }

        if (!policyTypesAdd.isEmpty()) {
            List<AreaPolicyTypes> policys = policyTypesAdd.stream()
                    .map(policyId -> new AreaPolicyTypes(area, policyId)).collect(Collectors.toList());
            areaPolicyTypesCRUDRepository.saveAll(policys);
        }
    }

    public void checkPolicyTypesDepArea(List<Long> policyTypeCodes, Validation validation) {
        if (policyTypeCodes != null && !policyTypeCodes.isEmpty() &&
                !policyTypeCodes.stream().allMatch(ptc -> ptc.equals(1L))) {
            validation.error(AreaErrorReason.NO_PRIMARY_AREA);
        }
    }

    /* К_УУ_5 1.
   Система проверяет, что в списке доступных для МУ существуют типы участка с переданными кодами
 */
    public List<MuAvailableAreaTypes> checkAndGetAreaTypesNotExistInMU(long muId, List<Long> areaTypeCodes, Validation validation) {
        List<MuAvailableAreaTypes> muAvailableAreaTypes = muAvailableAreaTypesRepository.findAreaTypes(muId);
        List<Long> availableAreaTypes = muAvailableAreaTypes.stream()
                .map(MuAvailableAreaTypes::getAreaType)
                .map(AreaType::getCode)
                .collect(Collectors.toList());
        areaTypeCodes.forEach(a -> {
            if (!availableAreaTypes.contains(a)) {
                validation.error(AreaErrorReason.AREA_TYPE_NOT_EXISTS_IN_MU,
                        new ValidationParameter("areaType", areaTypesCRUDRepository.findById(a).map(AreaType::getTitle).orElse(String.valueOf(a))));
            }
        });
        return muAvailableAreaTypes.stream()
                .filter(a -> areaTypeCodes.contains(a.getAreaType().getCode()))
                .collect(Collectors.toList());
    }

    public void checkAreaTypeAvailable(AreaType areaType, Validation validation) throws ContingentException {
        List<MuAvailableAreaTypes> availableAreaTypes = muAvailableAreaTypesRepository.findByAreaTypes(areaType);
        if (availableAreaTypes.isEmpty()) {
            throw new ContingentException(validation.error(AreaErrorReason.AREA_TYPE_NOT_AVAILABLE_FOR_MU,
                    new ValidationParameter("areaTypeTitle", areaType.getTitle())));
        }
    }

    public void checkAreaTypeCountLimits(long moId, long muId, AreaType areaType, Validation validation) throws ContingentException {
        if (areaType.getAreaCountLimit() != null) {
            if (areaType.getAreaCountLimit().equals(AreaTypeCountLimitEnum.MO.getCode())) {
                List<Area> areas = areaRepository.findAreas(moId, null, areaType.getCode(), null, true);
                if (areas.size() >= AreaTypeCountLimitEnum.MO.getLimit()) {
                    validation.error(AreaErrorReason.AREAS_NUMBER_LIMIT_EXCEEDED,
                            new ValidationParameter("areaTypeTitle", areaType.getTitle()),
                            new ValidationParameter("areaTypeLimit", AreaTypeCountLimitEnum.MO.getLimit()));
                }
            } else if (areaType.getAreaCountLimit().equals(AreaTypeCountLimitEnum.MU.getCode())) {
                List<Area> areas = areaRepository.findAreas(null, muId, areaType.getCode(), null, true);
                if (areas.size() >= AreaTypeCountLimitEnum.MU.getLimit()) {
                    validation.error(AreaErrorReason.AREAS_NUMBER_LIMIT_EXCEEDED,
                            new ValidationParameter("areaTypeTitle", areaType.getTitle()),
                            new ValidationParameter("areaTypeLimit", AreaTypeCountLimitEnum.MU.getLimit()));
                }
            }
        }
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
    }

    public void checkAutoAssignForAttachment(AreaType areaType, Boolean autoAssignForAttachment,
                                             Boolean attachByMedicalReason, Validation validation) {
        if (autoAssignForAttachment) {
            if (areaType.getMpguAvailable() != null
                    && !Boolean.TRUE.equals(areaType.getMpguAvailable())) {
                validation.error(AreaErrorReason.CANT_SET_AUTO_ASSIGN_FOR_ATTACHMENT,
                        new ValidationParameter("areaTypeCode", areaType.getCode()));
            }
            if (Boolean.TRUE.equals(attachByMedicalReason)) {
                validation.error(AreaErrorReason.AREA_FLAGS_INCORRECT);
            }
        }
    }

    public void checkAttachByMedicalReason(AreaType areaType, Boolean attachByMedicalReason, Validation validation) {
        if (attachByMedicalReason != null && areaType.getAttachByMedicalReason() != null &&
                !Objects.equals(attachByMedicalReason, areaType.getAttachByMedicalReason())) {
            validation.error(AreaErrorReason.ATTACH_BY_MEDICAL_REASON_INCORRECT,
                    new ValidationParameter("attachByMedicalReason", attachByMedicalReason),
                    new ValidationParameter("attachByMedicalReason", areaType.getAttachByMedicalReason()));
        }
    }
}
