package moscow.ptnl.contingent.area.service;

import moscow.ptnl.contingent.area.entity.area.AddressAllocationOrders;
import moscow.ptnl.contingent.area.entity.area.Area;
import moscow.ptnl.contingent.area.entity.area.MuAddlAreaTypes;
import moscow.ptnl.contingent.area.entity.area.MoAddress;
import moscow.ptnl.contingent.area.entity.nsi.AreaType;
import moscow.ptnl.contingent.area.entity.nsi.KindAreaTypeEnum;
import moscow.ptnl.contingent.area.entity.nsi.MUTypeAreaTypes;
import moscow.ptnl.contingent.area.error.AreaErrorReason;
import moscow.ptnl.contingent.area.error.ContingentException;
import moscow.ptnl.contingent.area.error.Validation;
import moscow.ptnl.contingent.area.error.ValidationParameter;
import moscow.ptnl.contingent.area.model.nsi.AvailableToCreateType;
import moscow.ptnl.contingent.area.repository.area.AddressAllocationOrderCRUDRepository;
import moscow.ptnl.contingent.area.repository.area.AreaCRUDRepository;
import moscow.ptnl.contingent.area.repository.area.AreaRepository;
import moscow.ptnl.contingent.area.repository.area.MoAddressCRUDRepository;
import moscow.ptnl.contingent.area.repository.area.MuAddlAreaTypesRepository;
import moscow.ptnl.contingent.area.repository.nsi.AreaTypesCRUDRepository;
import moscow.ptnl.contingent.area.repository.nsi.MUTypeAreaTypesRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import ru.mos.emias.contingent2.core.NotNsiAddress;
import ru.mos.emias.contingent2.core.NsiAddress;
import ru.mos.emias.contingent2.core.MuType;

import java.time.LocalDate;
import java.time.Month;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

@Component
public class AreaChecker {

    @Autowired
    private AreaTypesCRUDRepository areaTypesCRUDRepository;

    @Autowired
    private MUTypeAreaTypesRepository muTypeAreaTypesRepository;

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

    /* Система проверяет, что в справочнике «Типы участков» (AREA_TYPES) существует каждый входной параметр
    «ИД типа участка» с признаком архивности = 0.
    Иначе возвращает ошибку */
    public void checkAreaTypesExist(List<Long> areaTypes, Validation validation, String parameterCode) {
        areaTypes.forEach(a -> {
            Optional<AreaType> areaType = areaTypesCRUDRepository.findById(a);

            if (!areaType.isPresent() || Boolean.TRUE.equals(areaType.get().getArchive())) {
                validation.error(AreaErrorReason.AREA_TYPE_NOT_FOUND, new ValidationParameter(parameterCode, a));
            }
        });
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
                        new ValidationParameter("areatype", muAddlAreaType.getAreaType().getName()));
            }
        }
    }

    /* Система проверяет в шаблоне профиля МУ (MU_PROFILE_TEMPLATES) возможность добавления:
    •	ИД типа МУ (MU_TYPE_ID) = ИД типа МУ;
    •	ИД типа участка (AREA_TYPE_CODE) = ИД типа участка;
    •	Допустимость создания (AVAILABLE_TO_CREATE) = «Возможно» .
    Если запись с типом участка не найдена или AVAILABLE_TO_CREATE <> «Возможно» , то Система возвращает ошибку */
    public void checkMuTypeAreaTypeCreateAvailable(Long muTypeId, List<Long> areaTypes, Validation validation) {
        List<MUTypeAreaTypes> templates = muTypeAreaTypesRepository.findMuTypeAreaTypes(muTypeId, areaTypes, null);

        if (templates != null && !templates.isEmpty()) {
            templates.forEach(temp -> {
                if (!AvailableToCreateType.POSSIBLE.getValue().equals(temp.getAvailableToCreate())) {
                    validation.error(AreaErrorReason.CANT_CHANGE_AREA_TYPE,
                            new ValidationParameter("areaType", temp.getAreaType().getName()));
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
                        new ValidationParameter("areaType", area.getAreaType().getName()),
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
        List<MUTypeAreaTypes> muTypeAreaTypes = muTypeAreaTypesRepository.findMuTypeAreaTypes(
                muTypes.stream().map(MuType::getMuTypeId).collect(Collectors.toList()), primaryAreaTypeCodes, null);

        if (muTypeAreaTypes.stream()
                .noneMatch(t ->
                    AvailableToCreateType.ALLOWED.getValue().equals(t.getAvailableToCreate()) ||
                            (AvailableToCreateType.POSSIBLE.getValue().equals(t.getAvailableToCreate()) &&
                                    !muAddlAreaTypesRepository.findMuAddlAreaTypes(
                                            muTypes.stream()
                                                    .filter(m -> Objects.equals(t.getMuTypeId(), m.getMuTypeId()))
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

    public Area checkAndGetArea(long areaId, Validation validation) {
        Area area = areaCRUDRepository.findById(areaId).orElse(null);

        if (area == null) {
            validation.error(AreaErrorReason.AREA_NOT_FOUND, new ValidationParameter("areaId", areaId));
        }
        else if (area.getArchived()) {
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

    public void checkAreaExistsInMU(long muId, long areaTypeCode, int number, Long excludeAreaId, Validation validation) {
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
}
