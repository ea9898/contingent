package moscow.ptnl.contingent.area.service;

import moscow.ptnl.contingent.area.entity.area.AddressAllocationOrders;
import moscow.ptnl.contingent.area.entity.area.Area;
import moscow.ptnl.contingent.area.entity.area.AreaAddress;
import moscow.ptnl.contingent.area.entity.area.AreaMedicalEmployees;
import moscow.ptnl.contingent.area.entity.area.AreaPolicyTypes;
import moscow.ptnl.contingent.area.entity.area.AreaToAreaType;
import moscow.ptnl.contingent.area.entity.area.MoAddress;
import moscow.ptnl.contingent.area.entity.area.MoAvailableAreaTypes;
import moscow.ptnl.contingent.area.entity.area.MuAddlAreaTypes;
import moscow.ptnl.contingent.area.entity.area.MuAvailableAreaTypes;
import moscow.ptnl.contingent.area.error.AreaErrorReason;
import moscow.ptnl.contingent.area.model.area.AddressLevelType;
import moscow.ptnl.contingent.area.model.area.AddressWrapper;
import moscow.ptnl.contingent.area.model.area.NotNsiAddress;
import moscow.ptnl.contingent.area.model.area.NsiAddress;
import moscow.ptnl.contingent.area.transform.AddressRegistryBaseTypeCloner;
import moscow.ptnl.contingent.area.transform.SearchAreaAddress;
import moscow.ptnl.contingent.area.transform.SearchAreaAddressCloner;
import moscow.ptnl.contingent.area.util.Period;
import moscow.ptnl.contingent.domain.esu.event.AreaInfoEvent;
import moscow.ptnl.contingent.domain.esu.event.annotation.LogESU;
import moscow.ptnl.contingent.error.ContingentException;
import moscow.ptnl.contingent.error.Validation;
import moscow.ptnl.contingent.error.ValidationParameter;
import moscow.ptnl.contingent.nsi.domain.area.AreaType;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeCountLimitEnum;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeKindEnum;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeRelations;
import moscow.ptnl.contingent.nsi.domain.area.PolicyType;
import moscow.ptnl.contingent.nsi.domain.area.PolicyTypeEnum;
import moscow.ptnl.contingent.nsi.domain.area.PositionCode;
import moscow.ptnl.contingent.nsi.domain.area.PositionNom;
import moscow.ptnl.contingent.nsi.repository.AddressFormingElementRepository;
import moscow.ptnl.contingent.nsi.repository.AreaTypeRelationsRepository;
import moscow.ptnl.contingent.nsi.repository.AreaTypesCRUDRepository;
import moscow.ptnl.contingent.nsi.repository.BuildingRegistryRepository;
import moscow.ptnl.contingent.nsi.repository.PositionCodeRepository;
import moscow.ptnl.contingent.nsi.repository.PositionNomRepository;
import moscow.ptnl.contingent.repository.area.AddressAllocationOrderCRUDRepository;
import moscow.ptnl.contingent.repository.area.AreaAddressCRUDRepository;
import moscow.ptnl.contingent.repository.area.AreaAddressRepository;
import moscow.ptnl.contingent.repository.area.AreaCRUDRepository;
import moscow.ptnl.contingent.repository.area.AreaMedicalEmployeeCRUDRepository;
import moscow.ptnl.contingent.repository.area.AreaPolicyTypesCRUDRepository;
import moscow.ptnl.contingent.repository.area.AreaPolicyTypesRepository;
import moscow.ptnl.contingent.repository.area.AreaRepository;
import moscow.ptnl.contingent.repository.area.MoAddressCRUDRepository;
import moscow.ptnl.contingent.repository.area.MoAvailableAreaTypesRepository;
import moscow.ptnl.contingent.repository.area.MuAddlAreaTypesRepository;
import moscow.ptnl.contingent.repository.area.MuAvailableAreaTypesRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Component;
import ru.mos.emias.contingent2.address.AddressRegistryBaseType;
import ru.mos.emias.contingent2.area.types.SearchAreaRequest;
import ru.mos.emias.contingent2.core.AddMedicalEmployee;
import ru.mos.emias.contingent2.core.ChangeMedicalEmployee;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.Month;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import moscow.ptnl.util.CollectionsUtil;

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

    @Autowired
    private PositionCodeRepository positionCodeRepository;

    @Autowired
    private AreaAddressRepository areaAddressRepository;

    @Autowired
    private AreaTypeRelationsRepository areaTypeRelationsRepository;

    @Autowired
    private AddressRegistryBaseTypeCloner addressRegistryBaseTypeCloner;

    @Autowired
    private SearchAreaAddressCloner searchAreaAddressCloner;

    /* Система проверяет, что в справочнике «Типы участков» (AREA_TYPES) существует каждый входной параметр
    «ИД типа участка» с признаком архивности = 0.
    Иначе возвращает ошибку */
    public List<AreaType> checkAndGetAreaTypesExist(List<Long> areaTypes, Validation validation) {
        List<AreaType> result = new ArrayList<>();

        areaTypes.forEach(a -> {
            Optional<AreaType> areaType = areaTypesCRUDRepository.findById(a);

            if (!areaType.isPresent() || Boolean.TRUE.equals(areaType.get().getArchived())) {
                validation.error(AreaErrorReason.AREA_TYPE_NOT_FOUND, new ValidationParameter("areaTypeCode", a));
            } else {
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

        if (ageMin == null && ageMax == null && ageMinM == null && ageMaxM == null && ageMinW == null && ageMaxW == null) {
            // Если передано ни одно поле возрастного контингента
            return;
        }

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
        if (!(ageMin == null || ageMin >= ageMinAreaType) || !(ageMax == null || ageMax <= ageMaxAreaType)) {
            validation.error(AreaErrorReason.AREA_AGE_SETUP_EXCEEDED,
                    new ValidationParameter(paramMinCode, ageMin), new ValidationParameter(paramMaxCode, ageMax),
                    new ValidationParameter(paramMinCode, ageMinAreaType), new ValidationParameter(paramMaxCode, ageMaxAreaType));
        }
    }

    public void checkAreaDependsOnPrimaryAreaType(Area area, AreaType areaType, Validation validation) {
        if (area.getPrimaryAreaTypes().stream()
                .map(AreaToAreaType::getAreaType)
                .anyMatch(a -> Objects.equals(a, areaType))) {
            validation.error(AreaErrorReason.AREA_ALREADY_DEPENDS_ON_AREA_TYPE,
                    new ValidationParameter("area", area.getId()),
                    new ValidationParameter("areaType.title", areaType.getTitle()));
        }
    }

    public void checkAreaTypeRelations(AreaType dependentAreaType, AreaType primaryAreaType, Validation validation) {
        Optional<AreaTypeRelations>areaTypeRelations = areaTypeRelationsRepository.getByDependentAndPrimaryAreaTypes(dependentAreaType, primaryAreaType);
        if (!areaTypeRelations.isPresent()) {
            validation.error(AreaErrorReason.AREA_TYPE_RELATIONS_NOT_EXISTS,
                    new ValidationParameter("dependentAreaTypeTitle", dependentAreaType.getTitle()),
                    new ValidationParameter("primaryAreaTypeTitle", primaryAreaType.getTitle()));
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

    public void checkParametersChanged(Area area, Long muId, Integer number, String description,
                                       List<Long> primaryAreaTypeCodesAddIds, List<Long> primaryAreaTypeCodesDelIds,
                                       List<Long> policyTypesAddIds, List<Long> policyTypesDelIds,
                                       Integer ageMin, Integer ageMax, Integer ageMinM, Integer ageMaxM, Integer ageMinW,
                                       Integer ageMaxW, Validation validation) {
        // Система проверяет, что передан хотя бы один параметр для изменения, иначе возвращает ошибку
        if (muId == null && number == null && description == null && primaryAreaTypeCodesAddIds.isEmpty()
                && primaryAreaTypeCodesDelIds.isEmpty() && policyTypesAddIds.isEmpty() && policyTypesDelIds.isEmpty()
                && ageMin == null && ageMax == null && ageMinM == null && ageMaxM == null && ageMinW == null && ageMaxW == null) {

            validation.error(AreaErrorReason.NOTHING_TO_CHANGE);
            return;
        }
        // Система проверяет, что переданные параметры изменены, иначе возвращает ошибку
        if ((muId == null ? area.getMuId() == null : muId.equals(area.getMuId()))
                && (number == null ? area.getNumber() == null : number.equals(area.getNumber()))
                && (description == null ? area.getDescription() == null : description.equals(area.getDescription()))
                && primaryAreaTypeCodesAddIds.isEmpty() && primaryAreaTypeCodesDelIds.isEmpty()
                && policyTypesAddIds.isEmpty() && policyTypesDelIds.isEmpty()
                && (ageMin == null ? area.getAgeMin() == null : ageMin.equals(area.getAgeMin()))
                && (ageMax == null ? area.getAgeMax() == null : ageMax.equals(area.getAgeMax()))
                && (ageMinM == null ? area.getAgeMMin() == null : ageMinM.equals(area.getAgeMMin()))
                && (ageMaxM == null ? area.getAgeMMax() == null : ageMaxM.equals(area.getAgeMMax()))
                && (ageMinW == null ? area.getAgeWMin() == null : ageMinW.equals(area.getAgeWMin()))
                && (ageMaxW == null ? area.getAgeWMax() == null : ageMaxW.equals(area.getAgeWMax()))) {

            validation.error(AreaErrorReason.NOTHING_TO_CHANGE);
        }
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

    public void checkAreaExistsInMU(Long muId, long moId, AreaType areaType, Integer number, Long excludeAreaId, Validation validation) {
        List<Area> areas;
        if (muId == null) {
            areas = areaRepository.findAreas(moId, null, areaType.getCode(), number, true);
        } else {
            areas = areaRepository.findAreas(null, muId, areaType.getCode(), number, true);
        }
        if (areas.stream().anyMatch(a -> excludeAreaId == null || !Objects.equals(a.getId(), excludeAreaId))) {
            validation.error(AreaErrorReason.AREA_WITH_TYPE_AND_NUMBER_EXISTS_IN_MO,
                    new ValidationParameter("areaType", areaType.getTitle()),
                    new ValidationParameter("number", number));
        }
    }
    
    //Система проверяет, что передан хотя бы один параметр для изменения, иначе возвращает ошибку - С_УУ_101
    public void checkAreaParametersForUpdate(Integer number, 
            List<Long> policyTypesAddIds, List<Long> policyTypesDelIds, 
            Integer ageMin, Integer ageMax, Integer ageMinM, Integer ageMaxM, 
            Integer ageMinW, Integer ageMaxW, 
            Boolean autoAssignForAttachment, Boolean attachByMedicalReason, 
            String description,
            Validation validation) {
        if (number != null) 
            return;
        if (!CollectionsUtil.isNullOrEmpty(policyTypesAddIds))
            return;
        if (!CollectionsUtil.isNullOrEmpty(policyTypesDelIds))
            return;
        if (ageMin != null)
            return;
        if (ageMax != null)
            return;
        if (ageMinM != null)
            return;
        if (ageMaxM != null)
            return;
        if (ageMinW != null)
            return;
        if (ageMaxW != null)
            return;
        if (autoAssignForAttachment != null)
            return;
        if (attachByMedicalReason != null)
            return;
        if (description != null)
            return;
        
        validation.error(AreaErrorReason.NOTHING_TO_CHANGE);
    }
    
    //Система проверяет, что переданные параметры изменены, иначе возвращает ошибку.
    //Если значение входного параметра не соответствует значению, сохранённому в БД, значит данный параметр был изменен
    public void checkAreaParametersForUpdateChanged(Area area, Integer number, 
            List<PolicyType> policyTypesAdd, List<PolicyType> policyTypesDel, 
            Integer ageMin, Integer ageMax, Integer ageMinM, Integer ageMaxM, 
            Integer ageMinW, Integer ageMaxW, 
            Boolean autoAssignForAttachment, Boolean attachByMedicalReason, 
            String description, Validation validation) {
        
        if (!Objects.equals(area.getNumber(), number))
            return;
        if (!Objects.equals(area.getAgeMin(), ageMin))
            return;
        if (!Objects.equals(area.getAgeMax(), ageMax))
            return;
        if (!Objects.equals(area.getAgeMMin(), ageMinM))
            return;
        if (!Objects.equals(area.getAgeMMax(), ageMaxM))
            return;
        if (!Objects.equals(area.getAgeWMin(), ageMinW))
            return;
        if (!Objects.equals(area.getAgeWMax(), ageMaxW))
            return;
        if (!Objects.equals(area.getAutoAssignForAttach(), autoAssignForAttachment))
            return;
        if (!Objects.equals(area.getAttachByMedicalReason(), attachByMedicalReason))
            return;
        if (!Objects.equals(area.getDescription(), description))
            return;
        
        //если есть что удалять
        if (!CollectionsUtil.isNullOrEmpty(policyTypesDel)) {
            if (!areaPolicyTypesRepository.findAll(area, policyTypesDel).isEmpty()) 
                return;
        }
        
        //если есть что добавлять
        if (!CollectionsUtil.isNullOrEmpty(policyTypesAdd)) {
            if (!areaPolicyTypesRepository.findAll(area, policyTypesAdd)
                    .stream()
                    .map(apt -> apt.getPolicyType())
                    .collect(Collectors.toList())
                    .containsAll(policyTypesAdd)) {
                return;
            }            
        }        
        
        validation.error(AreaErrorReason.NOTHING_TO_CHANGE);
    }

    public void checkOrderExists(long orderId, Validation validation) {
        Optional<AddressAllocationOrders> order = addressAllocationOrderCRUDRepository.findById(orderId);

        if (!order.isPresent() || Boolean.TRUE.equals(order.get().getArchived())) {
            validation.error(AreaErrorReason.ADDRESS_ALLOCATION_ORDER_NOT_EXISTS, new ValidationParameter("orderId", orderId));
        }
    }

    public void checkTooManyAddresses(List<AddressRegistryBaseType> addresses, Long maxAddresses) throws ContingentException {
        if (addresses.size() > maxAddresses) {
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
                                    current.getMedicalEmployeeJobId()),
                            new ValidationParameter("specialization2",
                                    next.getMedicalEmployeeJobId()));
                }
            }
            if (!validation.isSuccess()) {
                throw new ContingentException(validation);
            }
        }
    }

    public List<AddressWrapper> convertAddressToWrapper(List<NsiAddress> nsiAddresses) {
        //Сформируем список адресов по справочнику
        return nsiAddresses.stream()
                .map(AddressWrapper::new)
                .peek(a -> {
                    if (!Objects.equals(a.nsiAddress.getLevelAddress(), AddressLevelType.ID.getLevel())) {
                        a.addressFormingElement = addressFormingElementRepository.getAddressFormingElements(
                                a.nsiAddress.getGlobalId(), a.nsiAddress.getLevelAddress()).get(0);
                    }
                    else {
                        a.buildingRegistry = buildingRegistryRepository.getBuildingsRegistry(a.nsiAddress.getGlobalId()).get(0);
                    }
                })
                .collect(Collectors.toList());
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
            allEmployees.sort(Comparator.comparing(AreaMedicalEmployees::getMedicalEmployeeJobId)
                    .thenComparing(AreaMedicalEmployees::getStartDate));
            for (int i = 0; i < allEmployees.size() - 1; i++) {
                AreaMedicalEmployees current = allEmployees.get(i);
                AreaMedicalEmployees next = allEmployees.get(i + 1);
                if (current.getMedicalEmployeeJobId() != null
                        && current.getMedicalEmployeeJobId().equals(next.getMedicalEmployeeJobId())
                        && (current.getEndDate() == null || next.getStartDate().isBefore(current.getEndDate().plusDays(1)))) {
                    validation.error(AreaErrorReason.JOB_ID_DATE_OVERLAP,
                            new ValidationParameter("jobInfoId", current.getMedicalEmployeeJobId()),
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

            empl.setUpdateDate(LocalDateTime.now());
        }
    }

    public void addNew(List<AreaMedicalEmployees> employees, List<AddMedicalEmployee> addEmployees, Area area) {
        addEmployees.forEach(empl -> employees.add(new AreaMedicalEmployees(
                empl.getMedicalEmployeeJobId(),
                area,
                empl.isIsReplacement(),
                empl.getStartDate(),
                empl.getEndDate(),
                empl.getSnils(),
                positionCodeRepository.getByCode(empl.getPositionCode()),
                LocalDateTime.now(),
                LocalDateTime.now(),
                empl.getSubdivisionId())));
    }

    public PositionNom getPositionNomByPositionCode(String positionCodeCode) {
        Optional<PositionCode> result = positionCodeRepository.getByCode(positionCodeCode);
        if (result.isPresent()) {
            PositionCode positionCode = result.get();
            Optional<PositionNom> positionNomOptional = positionNomRepository.getByPositionCodeId(positionCode.getGlobalId());
            if(positionNomOptional.isPresent()) {
                return positionNomOptional.get();
            }
        }
        return null;
    }

    /**
     * Система закрывает территории обслуживания участка
     * @param addresses
     */
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

    /**
     * Система закрывает территории обслуживания МО
     * @param addresses
     */
    public void delMoAddresses(List<MoAddress> addresses) {
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

    public void checkAreaTypeIsPrimary(AreaType areaType, Validation validation) {
        if (!isAreaTypePrimary(areaType)) {
            validation.error(AreaErrorReason.AREA_TYPE_IS_NOT_PRIMARY, new ValidationParameter("areaType", areaType.getTitle()));
        }
    }

    public void checkAreaTypeIsDependent(AreaType areaType, Validation validation) {
        if (!isAreaTypeDependent(areaType)) {
            validation.error(AreaErrorReason.AREA_TYPE_IS_NOT_DEPENDENT, new ValidationParameter("areaType", areaType.getTitle()));
        }
    }

    public boolean isAreaTypePrimary(AreaType areaType) {
        return areaType != null && areaType.getAreaTypeClass() != null &&
                Objects.equals(PRIMARY_AREA_TYPE_CLASS, areaType.getAreaTypeClass().getCode());
    }

    public boolean isAreaPrimary(Area area) {
        return isAreaTypePrimary(area.getAreaType());
    }

    public boolean isAreaTypeDependent(AreaType areaType) {
        return areaType != null && areaType.getAreaTypeClass() != null &&
                Objects.equals(DEPENDENT_AREA_TYPE_CLASS, areaType.getAreaTypeClass().getCode());
    }

    public boolean isAreaDependent(Area area) {
        return isAreaTypeDependent(area.getAreaType());
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
    public void checkAddressNotServiceByAreaType(Area area, List<AddressWrapper> addressWrapperList, Validation validation) throws ContingentException {
/*
        for (AddressWrapper addressWrapper: addressWrapperList) {
            if (addressWrapper.getNsiAddress() != null) {
                NsiAddress nsiAddress = addressWrapper.getNsiAddress();
                Long foundAreaId = algorithms.searchAreaByAddress(area.getMoId(), area.getAreaType(),
                        Collections.singletonList(nsiAddress));
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
            }
        }
*/
    }

    public List<AddressWrapper> convertToAddressWrapper(List<NsiAddress> nsiAddresses) {
        List<AddressWrapper> addressWrapperList = new ArrayList<>();
        if (nsiAddresses != null) {
            addressWrapperList.addAll(nsiAddresses.stream().map(AddressWrapper::new).collect(Collectors.toList()));
        }
        return addressWrapperList;
    }

    public void addMoAddressToAddressWrapper(Area area, List<AddressWrapper> addressWrapperList, Validation validation) throws ContingentException {

/*
        for (AddressWrapper addressWrapper: addressWrapperList) {
            MoAddress serviceDistrictMO = algorithms.searchServiceDistrictMOByAddress(area.getMoId(), area.getAreaType(), null,
                    addressWrapper.getNsiAddress() != null ? Collections.singletonList(addressWrapper.getNsiAddress()) : new ArrayList<>(),
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
                            AreaErrorReason.ADDRESS_NOT_SERVICED_MO_NSI,
                            new ValidationParameter("levelAddress", addressWrapper.getNsiAddress().getLevelAddress()),
                            new ValidationParameter("globalId", addressWrapper.getNsiAddress().getGlobalId()),
                            new ValidationParameter("moId", area.getMoId()));
                }
            }
        }
*/
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
            validation.error(AreaErrorReason.POLICY_TYPE_IS_INCORRECT);
            throw new ContingentException(validation);
        }
    }

    public void checkPolicyTypesDel(Area area, List<PolicyType> policyTypesDel, Validation validation) throws ContingentException {
        if (!policyTypesDel.isEmpty()) {
            for (PolicyType policy : policyTypesDel) {
                if (areaPolicyTypesRepository.findAll(area, policy).isEmpty()) {
                    validation.error(AreaErrorReason.POLICY_TYPE_NOT_SET_FOR_AREA,
                            new ValidationParameter("areaTypeCode", policy.getCode()),
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

    public void checkAreaTypeAvailable(long moId, Long muId, AreaType areaType, Validation validation) {
        List<?> availableAreaTypes = muId != null ? muAvailableAreaTypesRepository.findByAreaTypes(areaType, muId) :
                moAvailableAreaTypesRepository.findByAreaTypes(areaType, moId);

        if (availableAreaTypes.isEmpty()) {
            validation.error(AreaErrorReason.AREA_TYPE_NOT_AVAILABLE_FOR_MU, new ValidationParameter("areaTypeTitle", areaType.getTitle()));
        }
    }

    public void checkAreaTypeCountLimits(long moId, Long muId, AreaType areaType, Validation validation) throws ContingentException {
        if (areaType.getAreaCountLimit() != null) {
            if (areaType.getAreaCountLimit().equals(AreaTypeCountLimitEnum.MO.getCode())) {
                List<Area> areas = areaRepository.findAreas(moId, null, areaType.getCode(), null, true);
                if (areas.size() >= AreaTypeCountLimitEnum.MO.getLimit()) {
                    validation.error(AreaErrorReason.AREAS_NUMBER_LIMIT_EXCEEDED,
                            new ValidationParameter("areaTypeTitle", areaType.getTitle()),
                            new ValidationParameter("areaTypeLimit", AreaTypeCountLimitEnum.MO.getDescription()));
                }
            } else if (areaType.getAreaCountLimit().equals(AreaTypeCountLimitEnum.MU.getCode())) {
                List<Area> areas = areaRepository.findAreas(null, muId, areaType.getCode(), null, true);
                if (areas.size() >= AreaTypeCountLimitEnum.MU.getLimit()) {
                    validation.error(AreaErrorReason.AREAS_NUMBER_LIMIT_EXCEEDED,
                            new ValidationParameter("areaTypeTitle", areaType.getTitle()),
                            new ValidationParameter("areaTypeLimit", AreaTypeCountLimitEnum.MU.getDescription()));
                }
            }
        }
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
    }

    public void checkAutoAssignForAttachment(AreaType areaType, Boolean autoAssignForAttachment,
                                             Boolean attachByMedicalReason, Validation validation) {
        if (Boolean.TRUE.equals(autoAssignForAttachment)) {
            if (areaType.getMpguAvailable() != null
                    && !Boolean.TRUE.equals(areaType.getMpguAvailable())) {
                validation.error(AreaErrorReason.CANT_SET_AUTO_ASSIGN_FOR_ATTACHMENT,
                        new ValidationParameter("areaType", areaType.getTitle() ));
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

    @LogESU(type = AreaInfoEvent.class, useResult = true)
    public Set<Area> deleteMoAddresses(List<MoAddress> addresses) {
        List<AreaAddress> areaAddresses = areaAddressRepository.findAreaAddresses(addresses.stream()
                .map(MoAddress::getId)
                .collect(Collectors.toList()));
        Set<Area> areas = areaAddresses.stream().map(AreaAddress::getArea).collect(Collectors.toSet());
        delAreaAddresses(areaAddresses);
        delMoAddresses(addresses);

        //Возвращаем участки, адреса которых были удалены, для передачи в ЕСУ
        return areas;
    }

    public void checkEmptyMuId(Long muId, AreaType areaType) throws ContingentException {
        if (muId == null && (AreaTypeKindEnum.MILDLY_ASSOCIATED.equalsCode(areaType.getAreaTypeKind().getCode()) ||
                AreaTypeKindEnum.TREATMENT_ROOM_ASSOCIATED.equalsCode(areaType.getAreaTypeKind().getCode()))) {
            throw new ContingentException(AreaErrorReason.NO_MU_ID_PARAMETER);
        }
    }

    public void splitAddresses(List<AddressRegistryBaseType> addresses) {
        // 1.
        List<AddressRegistryBaseType> splitesAddresses = addresses.stream()
                .filter(ar -> ar.getAreaOMKTE().getCode().contains(";")).collect(Collectors.toList());
        splitesAddresses.forEach(sa -> {
            for (String saAreaOmkTe: sa.getAreaOMKTE().getCode().split(";")) {
                // 2.
                AddressRegistryBaseType addressRegistryBaseType = addressRegistryBaseTypeCloner.clone(sa);
                addressRegistryBaseType.getAreaOMKTE().setCode(saAreaOmkTe);
                // 3.
                if (sa.getRegionOMKTE().getCode().contains(";")) {
                    String regionOMKTE = sa.getRegionOMKTE().getCode();
                    addressRegistryBaseType.getRegionOMKTE().setCode(saAreaOmkTe.substring(0, 2) + regionOMKTE.substring(2));
                }
                addresses.add(addressRegistryBaseType);
            }
            addresses.remove(sa);
        });
    }

    public void checkSearchParameters(Long areaTypeClassCode, Long moId, List<Long> muIds, List<Long> areaTypeCodes,
                                      Integer number, String description, Boolean isArchived,
                                      List<SearchAreaRequest.MedicalEmployee> medicalEmployees,
                                      List<SearchAreaAddress> addresses) throws ContingentException {
        if (areaTypeClassCode == null && moId == null && muIds.isEmpty() && areaTypeCodes.isEmpty() && number == null
                && description == null && isArchived == null && medicalEmployees.isEmpty() && addresses.isEmpty()) {
            throw new ContingentException(AreaErrorReason.NO_SEARCH_PARAMETERS);
        }
    }

    public void checkSearchAreaAddresses(List<SearchAreaAddress> addresses) throws ContingentException {
        if (addresses.stream().anyMatch(addr -> addr.getAoLevel().equals(AddressLevelType.MOSCOW.getLevel()))) {
            throw new ContingentException(AreaErrorReason.INCORRECT_ADDRESS_LEVEL);
        }
        ListIterator<SearchAreaAddress> iter = addresses.listIterator();
        while (iter.hasNext()) {
            SearchAreaAddress current = iter.next();
            String[] areas = current.getAreaOMKTEcode().split(";");
            String[] regions = current.getRegionOMKTEcode().split(";");
            if (areas.length > 1) {
                iter.remove();
                for (String area : areas) {
                    SearchAreaAddress copy = new SearchAreaAddress(current);
                    copy.setAreaOMKTEcode(area);
                    String firstTwoDigits = area.substring(0, 2);
                    copy.setRegionOMKTEcode(Arrays.stream(regions).filter(
                            reg -> reg.startsWith(firstTwoDigits)).findFirst().get());
                    iter.add(current.copy());
                }
            }
        }
    }
}
