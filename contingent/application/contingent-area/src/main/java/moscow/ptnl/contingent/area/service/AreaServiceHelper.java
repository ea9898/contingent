package moscow.ptnl.contingent.area.service;

import moscow.ptnl.contingent.area.entity.area.AddressAllocationOrders;
import moscow.ptnl.contingent.area.entity.area.Area;
import moscow.ptnl.contingent.area.entity.area.AreaAddress;
import moscow.ptnl.contingent.area.entity.area.AreaMedicalEmployees;
import moscow.ptnl.contingent.area.entity.area.MuAddlAreaTypes;
import moscow.ptnl.contingent.area.entity.area.MoAddress;
import moscow.ptnl.contingent.area.entity.nsi.AreaType;
import moscow.ptnl.contingent.area.entity.nsi.KindAreaTypeEnum;
import moscow.ptnl.contingent.area.entity.nsi.MuTypeAreaTypes;
import moscow.ptnl.contingent.area.error.AreaErrorReason;
import moscow.ptnl.contingent.area.error.ContingentException;
import moscow.ptnl.contingent.area.error.Validation;
import moscow.ptnl.contingent.area.error.ValidationParameter;
import moscow.ptnl.contingent.area.model.nsi.AvailableToCreateType;
import moscow.ptnl.contingent.area.repository.area.AddressAllocationOrderCRUDRepository;
import moscow.ptnl.contingent.area.repository.area.AreaAddressCRUDRepository;
import moscow.ptnl.contingent.area.repository.area.AreaCRUDRepository;
import moscow.ptnl.contingent.area.repository.area.AreaMedicalEmployeeCRUDRepository;
import moscow.ptnl.contingent.area.repository.area.AreaRepository;
import moscow.ptnl.contingent.area.repository.area.MoAddressCRUDRepository;
import moscow.ptnl.contingent.area.repository.area.MuAddlAreaTypesRepository;
import moscow.ptnl.contingent.area.repository.nsi.AreaTypesCRUDRepository;
import moscow.ptnl.contingent.area.repository.nsi.MuTypeAreaTypesRepository;
import moscow.ptnl.contingent.area.repository.nsi.PositionNomClinicRepository;
import moscow.ptnl.contingent.area.util.Period;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Component;
import ru.mos.emias.contingent2.core.AddMedicalEmployee;
import ru.mos.emias.contingent2.core.ChangeMedicalEmployee;
import ru.mos.emias.contingent2.core.NotNsiAddress;
import ru.mos.emias.contingent2.core.NsiAddress;
import ru.mos.emias.contingent2.core.MuType;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.Month;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
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
    private MuTypeAreaTypesRepository muTypeAreaTypesRepository;

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
    private PositionNomClinicRepository positionNomClinicRepository;

    @Autowired
    private AreaAddressCRUDRepository areaAddressCRUDRepository;

    @Autowired
    private AreaMedicalEmployeeCRUDRepository areaMedicalEmployeeCRUDRepository;

    @Autowired
    private SettingService settingService;

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
                validation.error(AreaErrorReason.MU_PROFILE_EXISTS,
                        new ValidationParameter("muId", muAddlAreaType.getMuId()),
                        new ValidationParameter("areatype", muAddlAreaType.getAreaType().getTitle()));
            }
        }
    }

    /* Система проверяет в шаблоне профиля МУ (MU_PROFILE_TEMPLATES) возможность добавления:
    •	ИД типа МУ (MU_TYPE_ID) = ИД типа МУ;
    •	ИД типа участка (AREA_TYPE_CODE) = ИД типа участка;
    •	Допустимость создания (AVAILABLE_TO_CREATE) = «Возможно» .
    Если запись с типом участка не найдена или AVAILABLE_TO_CREATE <> «Возможно» , то Система возвращает ошибку */
    public void checkMuTypeAreaTypeCreateAvailable(Long muTypeId, List<Long> areaTypes, Validation validation) {
        List<MuTypeAreaTypes> templates = muTypeAreaTypesRepository.findMuTypeAreaTypes(muTypeId, areaTypes, null);

        if (templates != null && !templates.isEmpty()) {
            templates.forEach(temp -> {
                if (!AvailableToCreateType.POSSIBLE.getValue().equals(temp.getAvailableToCreate())) {
                    validation.error(AreaErrorReason.CANT_CHANGE_AREA_TYPE,
                            new ValidationParameter("areaType", temp.getAreaType().getTitle()));
                }
            });
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
            validation.error(AreaErrorReason.AREA_TYPES_NOT_EXISTS_IN_PROFILE,
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

    public void checkPrimaryAreaTypesForMuType(List<MuType> muTypes,
                                               List<Long> primaryAreaTypeCodes, Validation validation) {
        List<MuTypeAreaTypes> muTypeAreaTypes = muTypeAreaTypesRepository.findMuTypeAreaTypes(
                muTypes.stream().map(MuType::getMuTypeId).collect(Collectors.toList()), primaryAreaTypeCodes, null);

        if (muTypeAreaTypes.stream()
                .noneMatch(t ->
                    AvailableToCreateType.ALLOWED.getValue().equals(t.getAvailableToCreate()) ||
                            (AvailableToCreateType.POSSIBLE.getValue().equals(t.getAvailableToCreate()) &&
                                    !muAddlAreaTypesRepository.findMuAddlAreaTypes(
                                            muTypes.stream()
                                                    .filter(m -> Objects.equals(t.getMuTypeCode(), m.getMuTypeId()))
                                                    .map(MuType::getMuId)
                                                    .collect(Collectors.toList()),
                                            primaryAreaTypeCodes).isEmpty()))) {
            String areaTypes = String.join(", ", primaryAreaTypeCodes.stream().map(String::valueOf).collect(Collectors.toList()));
            validation.error(AreaErrorReason.MU_PROFILE_HAS_NO_AREA_TYPE, new ValidationParameter("primaryAreaTypeCode", areaTypes));
        }
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
        if (areaType.getKindAreaType() != null &&
                Objects.equals(areaType.getKindAreaType().getCode(), KindAreaTypeEnum.PERSONAL.getCode())) {
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

    public void noAddresses(List<NsiAddress> nsiAddresses,
                            List<NotNsiAddress> notNsiAddresses) throws ContingentException {
        if (nsiAddresses.size() + notNsiAddresses.size() == 0) {
            throw new ContingentException(AreaErrorReason.NO_ADDRESS);
        }
    }

    public void tooManyAddresses(List<NsiAddress> nsiAddresses,
                                 List<NotNsiAddress> notNsiAddresses, Long maxAddresses) throws ContingentException {
        if (nsiAddresses.size() + notNsiAddresses.size() > maxAddresses) {
            throw new ContingentException(AreaErrorReason.TOO_MANY_ADDRESSES);
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
                positionNomClinicRepository.getPositionProxy(empl.getPositionId()),
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
        return area.getAreaType() != null && area.getAreaType().getClassAreaType() != null &&
                Objects.equals(PRIMARY_AREA_TYPE_CLASS, area.getAreaType().getClassAreaType().getCode());
    }

    public boolean isAreaDependent(Area area) {
        return area.getAreaType() != null && area.getAreaType().getClassAreaType() != null &&
                Objects.equals(DEPENDENT_AREA_TYPE_CLASS, area.getAreaType().getClassAreaType().getCode());
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
}
