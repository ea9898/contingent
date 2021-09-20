package moscow.ptnl.contingent.domain.area.heplers;

import moscow.ptnl.contingent.domain.AreaErrorReason;
import moscow.ptnl.contingent.domain.area.entity.AddressAllocationOrders;
import moscow.ptnl.contingent.domain.area.entity.Addresses;
import moscow.ptnl.contingent.domain.area.entity.Area;
import moscow.ptnl.contingent.domain.area.entity.AreaAddress;
import moscow.ptnl.contingent.domain.area.entity.AreaMedicalEmployees;
import moscow.ptnl.contingent.domain.area.entity.AreaMuService;
import moscow.ptnl.contingent.domain.area.entity.AreaPolicyTypes;
import moscow.ptnl.contingent.domain.area.entity.AreaToAreaType;
import moscow.ptnl.contingent.domain.area.entity.MoAddress;
import moscow.ptnl.contingent.domain.area.entity.MoAvailableAreaTypes;
import moscow.ptnl.contingent.domain.area.entity.MuAvailableAreaTypes;
import moscow.ptnl.contingent.domain.area.model.area.AddMedicalEmployee;
import moscow.ptnl.contingent.domain.area.model.area.AddressLevelType;
import moscow.ptnl.contingent.domain.area.model.area.AddressRegistry;
import moscow.ptnl.contingent.domain.area.model.area.ChangeMedicalEmployee;
import moscow.ptnl.contingent.domain.area.model.area.MedicalEmployee;
import moscow.ptnl.contingent.domain.area.model.area.SearchAreaAddress;
import moscow.ptnl.contingent.domain.area.repository.AddressAllocationOrderRepository;
import moscow.ptnl.contingent.domain.area.repository.AddressesRepository;
import moscow.ptnl.contingent.domain.area.repository.AreaAddressRepository;
import moscow.ptnl.contingent.domain.area.repository.AreaMedicalEmployeeRepository;
import moscow.ptnl.contingent.domain.area.repository.AreaMuServiceRepository;
import moscow.ptnl.contingent.domain.area.repository.AreaPolicyTypesRepository;
import moscow.ptnl.contingent.domain.area.repository.AreaRepository;
import moscow.ptnl.contingent.domain.area.repository.MoAddressRepository;
import moscow.ptnl.contingent.domain.area.repository.MoAvailableAreaTypesRepository;
import moscow.ptnl.contingent.domain.area.repository.MuAvailableAreaTypesRepository;
import moscow.ptnl.contingent.domain.esu.event.AreaInfoEvent;
import moscow.ptnl.contingent.domain.esu.event.annotation.LogESU;
import moscow.ptnl.contingent.domain.util.Period;
import moscow.ptnl.contingent.error.ContingentException;
import moscow.ptnl.contingent.error.Validation;
import moscow.ptnl.contingent.error.ValidationParameter;
import moscow.ptnl.contingent.infrastructure.service.setting.SettingService;
import moscow.ptnl.contingent.nsi.domain.area.AreaType;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeClassEnum;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeCountLimitEnum;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeKindEnum;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeProfile;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeRelations;
import moscow.ptnl.contingent.nsi.domain.area.PolicyType;
import moscow.ptnl.contingent.nsi.domain.area.PolicyTypeEnum;
import moscow.ptnl.contingent.nsi.domain.repository.AreaTypeProfileRepository;
import moscow.ptnl.contingent.nsi.domain.repository.AreaTypeRelationsRepository;
import moscow.ptnl.contingent.nsi.domain.repository.AreaTypesRepository;
import moscow.ptnl.contingent.nsi.domain.repository.PositionCodeRepository;
import moscow.ptnl.util.CollectionsUtil;
import moscow.ptnl.util.Strings;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Component;

import javax.persistence.Column;
import javax.persistence.JoinColumn;
import javax.transaction.Transactional;
import java.lang.reflect.Field;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.Month;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

@Component
@Transactional
public class AreaHelper {

    private static final Long PRIMARY_AREA_TYPE_CLASS = 1L;
    private static final Long DEPENDENT_AREA_TYPE_CLASS = 2L;

    @Autowired
    private MoAvailableAreaTypesRepository moAvailableAreaTypesRepository;

    @Autowired
    private MuAvailableAreaTypesRepository muAvailableAreaTypesRepository;

    @Autowired
    private AreaTypesRepository areaTypesRepository;

    @Autowired
    private MoAddressRepository moAddressRepository;

    @Autowired
    private AddressAllocationOrderRepository addressAllocationOrderRepository;

    @Autowired
    private AreaAddressRepository areaAddressRepository;

    @Autowired
    private AreaRepository areaRepository;

    @Autowired
    private AreaTypeRelationsRepository areaTypeRelationsRepository;

    @Autowired
    private SettingService settingService;

    @Autowired
    private AreaPolicyTypesRepository areaPolicyTypesRepository;

    @Autowired
    private PositionCodeRepository positionCodeRepository;

    @Autowired
    private AddressesRepository addressesRepository;

    @Autowired
    private AreaMedicalEmployeeRepository areaMedicalEmployeeRepository;

    @Autowired
    private AreaTypeProfileRepository areaTypeProfileRepository;

    @Autowired
    private AreaMuServiceRepository areaMuServiceRepository;

    public List<AreaType> checkAndGetAreaTypesExist(List<Long> areaTypes, Validation validation) {
        List<AreaType> result = new ArrayList<>();

        areaTypes.stream().distinct().forEach(a -> {
            Optional<AreaType> areaType = areaTypesRepository.findById (a);

            if (!areaType.isPresent() || Boolean.TRUE.equals(areaType.get().getArchived())) {
                validation.error(AreaErrorReason.AREA_TYPE_NOT_FOUND, new ValidationParameter("areaTypeCode", a));
            } else {
                result.add(areaType.get());
            }
        });

        return result;
    }

    public List<AreaTypeProfile> checkAndGetAreaTypeProfiles(List<Long> areaTypeProfileCodes, AreaType areaType, Validation validation) {
        List<AreaTypeProfile> result = new ArrayList<>();

        areaTypeProfileCodes.forEach(a -> {
            Optional<AreaTypeProfile> areaTypeProfile = areaTypeProfileRepository.findByCodeAndAreaType(a, areaType.getCode());

            if (!areaTypeProfile.isPresent() || Boolean.TRUE.equals(areaTypeProfile.get().getArchived())) {
                validation.error(AreaErrorReason.AREA_TYPE_PROFILE_NOT_FOUND, new ValidationParameter("areaTypeTitle", areaType.getTitle()));
            } else {
                result.add(areaTypeProfile.get());
            }
        });

        return result;
    }

    public void checkAreaTypesExistInMO(long moId, List<AreaType> areaTypes, Validation validation) {
        List<AreaType> availableAreaTypes = moAvailableAreaTypesRepository.findAreaTypes(moId).stream()
                .map(MoAvailableAreaTypes::getAreaType)
                .collect(Collectors.toList());
        areaTypes.forEach(a -> {
            if (availableAreaTypes.contains(a)) {
                validation.error(AreaErrorReason.AREA_TYPE_ALREADY_EXISTS,
                        new ValidationParameter("areaTypeTitle", a.getTitle()));
            }
        });
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
                        new ValidationParameter("areaTypeTitle", areaTypesRepository.findById(a).map(AreaType::getTitle).orElse(String.valueOf(a))));
            }
        });
        return moAvailableAreaTypes.stream()
                .filter(a -> areaTypeCodes.contains(a.getAreaType().getCode()))
                .collect(Collectors.toList());
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
                        new ValidationParameter("areaTypeTitle", a.getTitle()));
            }
        });
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
                        new ValidationParameter("areaTypeTitle", areaTypesRepository.findById(a).map(AreaType::getTitle).orElse(String.valueOf(a))));
            }
        });
        return muAvailableAreaTypes.stream()
                .filter(a -> areaTypeCodes.contains(a.getAreaType().getCode()))
                .collect(Collectors.toList());
    }


    public void checkDateTillToday(LocalDate date, Validation validation) {
        if (date.isBefore(LocalDate.of(1970, Month.JANUARY, 1)) ||
                date.isAfter(LocalDate.now())) {
            validation.error(AreaErrorReason.DATE_IN_INCORRECT_PERIOD);
        }
    }

    public List<MoAddress> getAndCheckMoAddressesExist(List<Long> moAddressIds, Validation validation) {
        List<MoAddress> result = new ArrayList<>();

        moAddressIds.forEach(a -> {
            Optional<MoAddress> order = moAddressRepository.findById(a);

            if (!order.isPresent() ||
                    order.get().getEndDate() != null && order.get().getEndDate().isBefore(LocalDate.now())) {
                validation.error(AreaErrorReason.MO_ADDRESS_NOT_EXISTS, new ValidationParameter("addressId", a));
            }
            else {
                result.add(order.get());
            }
        });
        return result;
    }

    public void checkOrderExists(long orderId, Validation validation) {
        Optional<AddressAllocationOrders> order = addressAllocationOrderRepository.findById(orderId);

        if (!order.isPresent() || Boolean.TRUE.equals(order.get().getArchived())) {
            validation.error(AreaErrorReason.ADDRESS_ALLOCATION_ORDER_NOT_EXISTS, new ValidationParameter("orderId", orderId));
        }
    }

    /**
     * Для вызова из метода delMoAddress
     * @param addresses
     * @return
     */
    @LogESU(type = AreaInfoEvent.class, useResult = true, methodName = "delMoAddress")
    public Set<Area> deleteAreaAddress(List<AreaAddress> addresses) {
        Set<Area> areas = addresses.stream().map(AreaAddress::getArea).collect(Collectors.toSet());
        delAreaAddresses(addresses);
        //Возвращаем участки, адреса которых были удалены, для передачи в ЕСУ
        return areas;
    }

    /**
     * Для вызова из метода delMoAddressTotal
     * @param addresses
     * @return
     */
    @LogESU(type = AreaInfoEvent.class, useResult = true, methodName = "delMoAddressTotal")
    public Set<Area> deleteAreaAddressTotal(List<AreaAddress> addresses) {
        Set<Area> areas = addresses.stream().map(AreaAddress::getArea).collect(Collectors.toSet());
        delAreaAddresses(addresses);
        //Возвращаем участки, адреса которых были удалены, для передачи в ЕСУ
        return areas;
    }

    /**
     * Система закрывает территории обслуживания МО
     * @param addresses
     */
    public void delMoAddresses(List<MoAddress> addresses) {
        addresses.forEach(a -> {
            if (a.getStartDate() != null && a.getStartDate().equals(LocalDate.now())) {
                moAddressRepository.delete(a);
            }
            else {
                a.setEndDate(LocalDate.now().minusDays(1));
                a.setUpdateDate(LocalDateTime.now());
                moAddressRepository.save(a);
            }
        });
    }


    /**
     * Система закрывает территории обслуживания участка
     * @param addresses
     */
    public void delAreaAddresses(List<AreaAddress> addresses) {
        addresses.forEach(a -> {
            if (a.getStartDate() != null && a.getStartDate().equals(LocalDate.now())) {
                a.setEndDate(LocalDate.now().minusDays(1)); // чтоб адреса не попадали в актуальные в отправке в ЕСУ
                a.setUpdateDate(LocalDateTime.now());
                areaAddressRepository.delete(a);
            }
            else {
                a.setEndDate(LocalDate.now().minusDays(1));
            }
        });
    }

    // Система проверяет, что размер страницы, указанный во входных параметрах, не превышает максимально допустимое значение, указанное во внутреннем системном параметре (PAR_3).
    // Иначе возвращает ошибку
    public void checkMaxPage(PageRequest paging, Validation validation) {
        if (paging != null && paging.getPageSize() > settingService.getPar3()) {
            validation.error(AreaErrorReason.TOO_BIG_PAGE_SIZE, new ValidationParameter("pageSize", settingService.getPar3()));
        }
    }

    public void checkSearchDnParameters(Long moId, List<Long> muIds, List<Long> areaTypeCodes, Long areaTypeProfileCode, List<Long> servicedMuIds,
                                        List<Long> specializationCodes, List<Long> areaIds) throws ContingentException {
        if (moId == null && muIds.isEmpty() && areaTypeCodes.isEmpty() && areaTypeProfileCode == null && servicedMuIds.isEmpty() &&
                specializationCodes.isEmpty() && areaIds.isEmpty()) {
            throw new ContingentException(AreaErrorReason.NO_SEARCH_PARAMETERS);
        }
    }

    public void checkSearchDnParameters(Long moId, List<Long> muIds, List<Long> areaTypeCodes, List<Long> specializationCodes, List<Long> areaIds) throws ContingentException {
        if (moId == null && muIds.isEmpty() && areaTypeCodes.isEmpty() && specializationCodes.isEmpty() && areaIds.isEmpty()) {
            throw new ContingentException(AreaErrorReason.NO_SEARCH_PARAMETERS);
        }
    }

    public void checkSearchParameters(Long areaTypeClassCode, Long moId, List<Long> muIds, List<Long> areaTypeCodes,
                                      Integer number, String description, Boolean isArchived,
                                      List<MedicalEmployee> medicalEmployees,
                                      List<SearchAreaAddress> addresses) throws ContingentException {
        if (areaTypeClassCode == null && moId == null && muIds.isEmpty() && areaTypeCodes.isEmpty() && number == null
                && description == null && isArchived == null && medicalEmployees.isEmpty() && addresses.isEmpty()) {
            throw new ContingentException(AreaErrorReason.NO_SEARCH_PARAMETERS);
        }
    }

    public void checkSearchParametersV2(Long areaTypeClassCode, Long moId, List<Long> muIds, List<Long> areaTypeCodes,
                                        Long areaTypeProfile, List<Long> servicedMuIds, Integer number, String description,
                                        Boolean isArchived, List<MedicalEmployee> medicalEmployees, List<SearchAreaAddress> addresses) throws ContingentException {
        if (areaTypeClassCode == null && moId == null && muIds.isEmpty() && areaTypeCodes.isEmpty() && areaTypeProfile == null && number == null
                && description == null && isArchived == null && medicalEmployees.isEmpty() && addresses.isEmpty() && servicedMuIds.isEmpty()) {
            throw new ContingentException(AreaErrorReason.NO_SEARCH_PARAMETERS);
        }
    }

    public void checkSearchAreaInaccurateAddress(Boolean exactAddressMatch, List<SearchAreaAddress> addresses) throws ContingentException {
        if (Boolean.FALSE.equals(exactAddressMatch) && addresses.size() > 1) {
            throw new ContingentException(AreaErrorReason.SEARCH_AREA_INACCURATE_ADDRESS_ERROR);
        }
    }

    public void checkSearchAreaAddresses(List<SearchAreaAddress> addresses) throws ContingentException {
        if (addresses.stream().anyMatch(addr -> AddressLevelType.MOSCOW.getLevel().equals(addr.getAoLevel()))) {
            throw new ContingentException(AreaErrorReason.INCORRECT_ADDRESS_LEVEL,
                    new ValidationParameter("aoLevel", AddressLevelType.MOSCOW.getLevel()));
        }
        ListIterator<SearchAreaAddress> iter = addresses.listIterator();
        while (iter.hasNext()) {
            SearchAreaAddress current = iter.next();
            String[] regions = current.getRegionOMKTEcode().split(";");
            if (current.getAreaOMKTEcode() != null) {
                String[] areas = current.getAreaOMKTEcode().split(";");
                if (areas.length > 1 || regions.length > 1) {
                    iter.remove();
                    for (String area : areas) {
                        SearchAreaAddress copy = new SearchAreaAddress(current);
                        copy.setAreaOMKTEcode(area);
                        String firstTwoDigits = area.substring(0, 2);
                        Optional<String> region = Arrays.stream(regions).filter(
                                reg -> reg.startsWith(firstTwoDigits)).findFirst();
                        region.ifPresent(copy::setRegionOMKTEcode);
                        iter.add(copy);
                    }
                }
            } else if (regions.length > 1){
                iter.remove();
                for (String region : regions) {
                    SearchAreaAddress copy = new SearchAreaAddress(current);
                    copy.setRegionOMKTEcode(region);
                    iter.add(copy);
                }
            }
        }
    }

    public void checkAreaTypeIsPrimary(AreaType areaType, Validation validation) {
        if (!isAreaTypePrimary(areaType)) {
            validation.error(AreaErrorReason.AREA_TYPE_IS_NOT_PRIMARY, new ValidationParameter("areaTypeTitle", areaType.getTitle()));
        }
    }

    public boolean isAreaTypePrimary(AreaType areaType) {
        return areaType != null && areaType.getAreaTypeClass() != null &&
                Objects.equals(PRIMARY_AREA_TYPE_CLASS, areaType.getAreaTypeClass().getCode());
    }

    public void checkEmptyMuId(Long muId, AreaType areaType) throws ContingentException {
        if (muId == null && (AreaTypeKindEnum.MILDLY_ASSOCIATED.equalsCode(areaType.getAreaTypeKind().getCode()) ||
                AreaTypeKindEnum.TREATMENT_ROOM_ASSOCIATED.equalsCode(areaType.getAreaTypeKind().getCode()))) {
            throw new ContingentException(AreaErrorReason.NO_MU_ID_PARAMETER);
        }
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

    public void checkAreaExistsInMU(Long muId, long moId, AreaType areaType, Integer number, Long excludeAreaId, Validation validation) {
        List<Area> areas;
        if (muId == null) {
            areas = areaRepository.findAreas(moId, null, areaType.getCode(), number, true);
        } else {
            areas = areaRepository.findAreas(null, muId, areaType.getCode(), number, true);
        }
        if (areas.stream().anyMatch(a -> excludeAreaId == null || !Objects.equals(a.getId(), excludeAreaId))) {
            validation.error(AreaErrorReason.AREA_WITH_TYPE_AND_NUMBER_EXISTS_IN_MO,
                    new ValidationParameter("areaTypeTitle", areaType.getTitle()),
                    new ValidationParameter("areaNumber", number));
        }
    }

    public void checkPolicyTypesIsOMS(List<Long> policyTypesAdd,  Validation validation) throws ContingentException {
        if (!policyTypesAdd.isEmpty() && policyTypesAdd.stream().anyMatch(policy -> !policy.equals(PolicyTypeEnum.OMS.getCode()))) {
            validation.error(AreaErrorReason.POLICY_TYPE_IS_INCORRECT);
            throw new ContingentException(validation);
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
        return (ageMin != null && ageMinAreaType == null) || (ageMin == null && ageMinAreaType != null) ||
                (ageMax != null && ageMaxAreaType == null) || (ageMax == null && ageMaxAreaType != null);
    }

    public void checkAgeSetupRange(Integer ageMin, Integer ageMax, Integer ageMinAreaType, Integer ageMaxAreaType,
                                   String paramMinCode, String paramMaxCode, Validation validation) {
        if (!(ageMin == null || ageMin >= ageMinAreaType) || !(ageMax == null || ageMax <= ageMaxAreaType)) {
            validation.error(AreaErrorReason.AREA_AGE_SETUP_EXCEEDED,
                    new ValidationParameter(paramMinCode + "1", ageMin), new ValidationParameter(paramMaxCode + "1", ageMax),
                    new ValidationParameter(paramMinCode + "2", ageMinAreaType), new ValidationParameter(paramMaxCode + "2", ageMaxAreaType));
        }
    }

    public void checkAutoAssignForAttachment(AreaType areaType, Boolean autoAssignForAttachment,
                                             Boolean attachByMedicalReason, Validation validation) {
        if (Boolean.TRUE.equals(autoAssignForAttachment)) {
            if (areaType.getMpguAvailable() != null
                    && !Boolean.TRUE.equals(areaType.getMpguAvailable())) {
                validation.error(AreaErrorReason.CANT_SET_AUTO_ASSIGN_FOR_ATTACHMENT,
                        new ValidationParameter("areaTypeTitle", areaType.getTitle() ));
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
                    new ValidationParameter("attachByMedicalReason1", attachByMedicalReason),
                    new ValidationParameter("attachByMedicalReason2", areaType.getAttachByMedicalReason()));
        }
    }

    public void checkAreaTypeRelations(AreaType dependentAreaType, AreaType primaryAreaType, Validation validation) {
        Optional<AreaTypeRelations>areaTypeRelations = areaTypeRelationsRepository.getByDependentAndPrimaryAreaTypes(dependentAreaType, primaryAreaType);
        if (!areaTypeRelations.isPresent())
        {
            validation.error(AreaErrorReason.AREA_TYPE_RELATIONS_NOT_EXISTS,
                    new ValidationParameter("dependentAreaTypeTitle", dependentAreaType.getTitle()),
                    new ValidationParameter("primaryAreaTypeTitle", primaryAreaType.getTitle()));
        }
    }

    public void checkAreaTypeIsDependent(AreaType areaType, Validation validation) {
        if (!isAreaTypeDependent(areaType)) {
            validation.error(AreaErrorReason.AREA_TYPE_IS_NOT_DEPENDENT, new ValidationParameter("areaTypeTitle", areaType.getTitle()));
        }
    }

    public boolean isAreaTypeDependent(AreaType areaType) {
        return areaType != null && areaType.getAreaTypeClass() != null &&
                Objects.equals(DEPENDENT_AREA_TYPE_CLASS, areaType.getAreaTypeClass().getCode());
    }


    public Map<AreaMedicalEmployees, AreaMedicalEmployees> applyChanges(List<AreaMedicalEmployees> employees, List<ChangeMedicalEmployee> changeEmployees) {
        Map<AreaMedicalEmployees, AreaMedicalEmployees> historyMap = new HashMap<>();

        for (AreaMedicalEmployees empl : employees) {
            AreaMedicalEmployees medicalEmployeeOld = empl.clone();

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
            empl.setError(changeEmpl.isIsError());

            historyMap.put(medicalEmployeeOld, empl);
        }

        return historyMap;
    }

    // Проверяет существует ли участок с указанным идентификатором и не находится ли он в архиве
    public Area checkAndGetArea(long areaId, Validation validation) {
        Area area = areaRepository.findById(areaId).orElse(null);

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

    public void checkPolicyTypesDel(Area area, List<PolicyType> policyTypesDel, Validation validation) throws ContingentException {
        if (!policyTypesDel.isEmpty()) {
            for (PolicyType policy : policyTypesDel) {
                if (areaPolicyTypesRepository.findAll(area, policy).isEmpty()) {
                    validation.error(AreaErrorReason.POLICY_TYPE_NOT_SET_FOR_AREA,
                            new ValidationParameter("policyCode", policy.getCode()),
                            new ValidationParameter("areaId", area.getId()));
                }
            }
            if (!validation.isSuccess()) {
                throw new ContingentException(validation);
            }
        }
    }

    public void resetAutoAssignForAttachment(Area area) {
        if (area.getAutoAssignForAttach()) {
            List<Area> areas = areaRepository.findAreas(null, area.getMuId(), area.getAreaType().getCode(), null, null);
            areas.stream().filter(a -> !Objects.equals(area.getId(), a.getId())).forEach(a -> a.setAutoAssignForAttach(false));
        }
    }

    public void saveAndDeleteAreaPolicyTypes(Area area, List<PolicyType> policyTypesAdd, List<PolicyType> policyTypesDel) throws ContingentException {

        if (!policyTypesDel.isEmpty()) {
            areaPolicyTypesRepository.deleteAll(area, policyTypesDel);
        }

        if (!policyTypesAdd.isEmpty()) {
            List<AreaPolicyTypes> policys = policyTypesAdd.stream()
                    .map(policyId -> new AreaPolicyTypes(area, policyId)).collect(Collectors.toList());
            areaPolicyTypesRepository.saveAll(policys);
        }
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

    public void checkAreaDependsOnPrimaryAreaType(Area area, AreaType areaType, Validation validation) {
        if (area.getPrimaryAreaTypes().stream()
                .map(AreaToAreaType::getAreaType)
                .anyMatch(a -> Objects.equals(a, areaType))) {
            validation.error(AreaErrorReason.AREA_ALREADY_DEPENDS_ON_AREA_TYPE,
                    new ValidationParameter("areaId", area.getId()),
                    new ValidationParameter("areTypeTitle", areaType.getTitle()));
        }
    }

    public List<AreaMedicalEmployees> addNew(List<AreaMedicalEmployees> employees, List<AddMedicalEmployee> addEmployees, Area area) {
        List<AreaMedicalEmployees> newAME = new ArrayList<>();
        addEmployees.forEach(empl -> {
            AreaMedicalEmployees medicalEmployees = new AreaMedicalEmployees();
            medicalEmployees.setMedicalEmployeeJobId(empl.getMedicalEmployeeJobInfoId());
            medicalEmployees.setArea(area);
            medicalEmployees.setReplacement(empl.isReplacement());
            medicalEmployees.setStartDate(empl.getStartDate());
            medicalEmployees.setEndDate(empl.getEndDate());
            medicalEmployees.setSnils(empl.getSnils());
            if (!Strings.isNumberWith4Digits(empl.getPositionCode())) {
                medicalEmployees.setPositionCode(empl.getPositionCode());
            } else {
                medicalEmployees.setPositionCodeSupp(Long.parseLong(empl.getPositionCode()));
            }
            medicalEmployees.setCreateDate(LocalDateTime.now());
            medicalEmployees.setUpdateDate(LocalDateTime.now());
            medicalEmployees.setSubdivisionId(empl.getSubdivisionId());
            employees.add(medicalEmployees);
            newAME.add(medicalEmployees.clone());
        });
        return newAME;
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
        mainEmployees = mainEmployees.stream().filter(empl -> empl.getStartDate() != null
                && !(empl.getStartDate().isBefore(LocalDate.now().plusDays(1))
                && empl.getEndDate() != null
                && empl.getEndDate().isBefore(LocalDate.now().plusDays(1))))
                .collect(Collectors.toList());
        mainEmployees.sort(Comparator.comparing(AreaMedicalEmployees::getStartDate));
        List<Period> periodsWithoutMainEmpl = new ArrayList<>();
        if (mainEmployees.isEmpty()) {
            periodsWithoutMainEmpl.add(new Period(LocalDate.now(), Period.MAX_DATE));
            return periodsWithoutMainEmpl;
        }
        AreaMedicalEmployees first = mainEmployees.get(0);
        if (first.getStartDate().isAfter(LocalDate.now())) {
            periodsWithoutMainEmpl.add(new Period(LocalDate.now(), first.getStartDate().minusDays(1)));
        }
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
            if (last.getEndDate().isAfter(LocalDate.now())) {
                periodsWithoutMainEmpl.add(new Period(last.getEndDate().plusDays(1), Period.MAX_DATE));
            } else {
                periodsWithoutMainEmpl.add(new Period(LocalDate.now(), Period.MAX_DATE));
            }
        }
        return periodsWithoutMainEmpl;
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
                            new ValidationParameter("JobInfoId1",
                                    current.getMedicalEmployeeJobId()),
                            new ValidationParameter("JobInfoId2",
                                    next.getMedicalEmployeeJobId()));
                }
            }
            if (!validation.isSuccess()) {
                throw new ContingentException(validation);
            }
        }
    }

    public boolean isAreaDependent(Area area) {
        return isAreaTypeDependent(area.getAreaType());
    }

    public void checkTooManyAddresses(List<AddressRegistry> addresses, Long maxAddresses) throws ContingentException {
        if (addresses.size() > maxAddresses) {
            throw new ContingentException(AreaErrorReason.TOO_MANY_ADDRESSES, new ValidationParameter("maxAddresses", maxAddresses));
        }
    }

    public List<Addresses> getMoAreaAddresses(List<Addresses> addressesInput) {
        List<Long> addressIds = addressesInput.stream().map(Addresses::getGlobalId).collect(Collectors.toList());
        List<Addresses> addresses = addressIds.isEmpty() ? Collections.emptyList() : addressesRepository.findAddresses(addressIds);
        addressesRepository.saveAll(addressesInput.stream().filter(ai -> !addresses.stream().map(Addresses::getGlobalId).collect(Collectors.toList())
                .contains(ai.getGlobalId())).collect(Collectors.toList())).forEach(addresses::add);
        return addresses;
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

    public Area checkAndGetArchivedArea(long areaId, Validation validation) {
        Area area = areaRepository.findById(areaId).orElse(null);

        if (area == null) {
            validation.error(AreaErrorReason.AREA_NOT_FOUND, new ValidationParameter("areaId", areaId));
        }
        else if (!area.getArchived()) {
            validation.error(AreaErrorReason.AREA_IS_NOT_ARCHIVED, new ValidationParameter("areaId", areaId));
        }
        return area;
    }


    public void delAreaMedicalEmployees(List<AreaMedicalEmployees> employees) {
        employees.stream().filter(me -> me.getError() == null || !me.getError()).forEach(a -> {
            if (a.getStartDate().equals(LocalDate.now())) {
                a.setEndDate(LocalDate.now().minusDays(1)); // что б МР не попадали в актуальные при отправке в ЕСУ
                areaMedicalEmployeeRepository.delete(a);
            }
            else {
                a.setEndDate(LocalDate.now().minusDays(1));
            }
        });
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


    public List<AddressRegistry> filterDistinctAddressesByGlobalId(List<AddressRegistry> addresses) throws ContingentException {
        Set<Long> exist = new HashSet<>();
        return addresses.stream()
                .filter(a -> a.getGlobalIdNsi() == null || exist.add(a.getGlobalIdNsi()))
                .collect(Collectors.toList());
    }

    public boolean isAreaPrimary(Area area) {
        return isAreaTypePrimary(area.getAreaType());
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

    public Field findTableField(String fieldName, Object object) {
        for (Field field : object.getClass().getDeclaredFields()) {
            Column column = field.getAnnotation(Column.class);

            if (column != null && Objects.equals(fieldName, column.name())) {
                return field;
            }
            JoinColumn joinColumn = field.getAnnotation(JoinColumn.class);

            if (joinColumn != null && Objects.equals(fieldName, joinColumn.name())) {
                return field;
            }
        }
        return null;
    }

    public void checkMuAlreadyServiced(Area area, List<Long> servicedMuIds, Validation validation) {
        for (Long servicedMuId : servicedMuIds) {
            if (area.getAreaTypeProfile() != null) {
                List<AreaMuService> areaMuServices = areaMuServiceRepository.findActive(servicedMuId, area.getId(), area.getAreaTypeProfile().getCode());
                if (!areaMuServices.isEmpty()) {
                    validation.error(AreaErrorReason.MU_ALREADY_SERVICED,
                            new ValidationParameter("muService/muId", servicedMuId),
                            new ValidationParameter("areaId", areaMuServices.stream()
                                    .map(AreaMuService::getArea)
                                    .map(Area::getId)
                                    .map(String::valueOf)
                                    .collect(Collectors.joining(";")))
                    );
                }
            }
        }
    }

    //Система проверяет, что количество переданных на вход адресов не превышает максимально допустимое значение,
    // указанное во внутреннем системном параметре
    public void checkMaxRequestAddresses(int numberOfAddresses, Validation validation) {
        if (numberOfAddresses > settingService.getPar42()) {
            validation.error(AreaErrorReason.TOO_MANY_REQUEST_ADDRESSES, new ValidationParameter("maxAddresses", settingService.getPar42()));
        }
    }

    //Система проверяет, что каждый переданный «Код типа участка» является первичным (AREA_TYPE.AREA_TYPE_CLASS_CODE = 1)
    public void checkAreaTypesArePrimary(List<AreaType> areaTypes, Validation validation) {
        areaTypes.stream()
                .filter(a -> !AreaTypeClassEnum.PRIMARY.areaTypeClassEquals(a.getAreaTypeClass()))
                .forEach(a -> validation.error(AreaErrorReason.AREA_TYPE_IS_NOT_PRIMARY_2,
                        new ValidationParameter("areaTypeCode", a.getCode())));
    }

    //Система проверяет, что каждый переданный «Код типа участка» имеет признак "обслуживает территорию" (AREA_TYPE.HAS_SERVICE_TERRITORY = 1)
    public void checkAreaTypesServeTerritory(List<AreaType> areaTypes, Validation validation) {
        areaTypes.stream()
                .filter(a -> !Boolean.TRUE.equals(a.getHasServiceTerritory()))
                .forEach(a -> validation.error(AreaErrorReason.AREA_TYPE_DO_NOT_SERVES_TERRITORY,
                        new ValidationParameter("areaTypeCode", a.getCode())));
    }
}
