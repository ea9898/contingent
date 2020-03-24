package moscow.ptnl.contingent.domain.area;

import moscow.ptnl.contingent.domain.AreaErrorReason;
import moscow.ptnl.contingent.domain.area.entity.AddressAllocationOrders;
import moscow.ptnl.contingent.domain.area.entity.Area;
import moscow.ptnl.contingent.domain.area.entity.AreaAddress;
import moscow.ptnl.contingent.domain.area.entity.MoAddress;
import moscow.ptnl.contingent.domain.area.entity.MoAvailableAreaTypes;
import moscow.ptnl.contingent.domain.area.entity.MuAvailableAreaTypes;
import moscow.ptnl.contingent.domain.area.repository.AddressAllocationOrderRepository;
import moscow.ptnl.contingent.domain.area.repository.AreaAddressRepository;
import moscow.ptnl.contingent.domain.area.repository.MoAddressRepository;
import moscow.ptnl.contingent.domain.area.repository.MoAvailableAreaTypesRepository;
import moscow.ptnl.contingent.domain.area.repository.MuAvailableAreaTypesRepository;
import moscow.ptnl.contingent.domain.esu.event.AreaInfoEvent;
import moscow.ptnl.contingent.domain.esu.event.annotation.LogESU;
import moscow.ptnl.contingent.error.ContingentException;
import moscow.ptnl.contingent.error.Validation;
import moscow.ptnl.contingent.error.ValidationParameter;
import moscow.ptnl.contingent.infrastructure.service.setting.SettingService;
import moscow.ptnl.contingent.nsi.domain.area.AreaType;
import moscow.ptnl.contingent.nsi.domain.repository.AreaTypesRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Component;

import javax.transaction.Transactional;
import java.time.LocalDate;
import java.time.Month;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

@Component
@Transactional
public class AreaHelper {

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
    private SettingService settingService;

    public List<AreaType> checkAndGetAreaTypesExist(List<Long> areaTypes, Validation validation) {
        List<AreaType> result = new ArrayList<>();

        areaTypes.forEach(a -> {
            Optional<AreaType> areaType = areaTypesRepository.findById (a);

            if (!areaType.isPresent() || Boolean.TRUE.equals(areaType.get().getArchived())) {
                validation.error(AreaErrorReason.AREA_TYPE_NOT_FOUND, new ValidationParameter("areaTypeCode", a));
            } else {
                result.add(areaType.get());
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
     * Такое название из
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

    public void checkSearchDnParameters(Long moId, List<Long> muIds, List<Long> areaTypeCodes, List<Long> specializationCodes, List<Long> areaIds) throws ContingentException {
        if (moId == null && muIds.isEmpty() && areaTypeCodes.isEmpty() && specializationCodes.isEmpty() && areaIds.isEmpty()) {
            throw new ContingentException(AreaErrorReason.NO_SEARCH_PARAMETERS);
        }
    }
}
