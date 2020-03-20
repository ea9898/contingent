package moscow.ptnl.contingent.area.service;

import moscow.ptnl.contingent.domain.area.entity.Addresses;
import moscow.ptnl.contingent.domain.area.model.area.AddressLevelType;
import org.springframework.stereotype.Component;
import ru.mos.emias.contingent2.address.AddressRegistryBaseType;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.function.BiFunction;
import java.util.function.BiPredicate;
import java.util.stream.Collectors;

@Component
public class AlgorithmsHelper {

    static final String ADDRESS_CODE_VALUES_SPLITTER = ";";

    // REGION_TE_CODE  LIKE '%REGION_TE_CODE%'
    // AREACODE_OMK_TE  LIKE '%AREACODE_OMK_TE%'
    public static BiPredicate<AddressRegistryBaseType, Addresses> regionTeAndAreaOmkTeFilter = (addressRegistry, addr) ->
            (addressRegistry.getRegionOMKTE() == null || (addr.getRegionTeCode() != null && addressRegistry.getRegionOMKTE().getCode().contains(addr.getRegionTeCode()))) &&
            (addressRegistry.getAreaOMKTE() == null || (addr.getAreaCodeOmkTe() != null && addressRegistry.getAreaOMKTE().getCode().contains(addr.getAreaCodeOmkTe())));

    // AREACODE = AREACODE
    public static BiPredicate<AddressRegistryBaseType, Addresses> areaCodeFilter = (addressRegistry, addr) ->
            regionTeAndAreaOmkTeFilter.test(addressRegistry, addr) &&
            (addressRegistry.getArea() == null || addressRegistry.getArea().getCode().equals(addr.getAreaCode()));

    // CITYCODE = CITYCODE
    public static BiPredicate<AddressRegistryBaseType, Addresses> cityCodeFilter = (addressRegistry, addr) ->
            areaCodeFilter.test(addressRegistry, addr) &&
            (addressRegistry.getCity() == null || addressRegistry.getCity().getCode().equals(addr.getCityCode()));

    // PLACECODE = PLACECODE
    public static BiPredicate<AddressRegistryBaseType, Addresses> placeCodeFilter = (addressRegistry, addr) ->
            cityCodeFilter.test(addressRegistry, addr) &&
            (addressRegistry.getPlace() == null || addressRegistry.getPlace().getCode().equals(addr.getPlaceCode()));

    // PLANCODE = PLANCODE
    public static BiPredicate<AddressRegistryBaseType, Addresses> planCodeFilter = (addressRegistry, addr) ->
            placeCodeFilter.test(addressRegistry, addr) &&
            (addressRegistry.getPlan() == null || addressRegistry.getPlan().getCode().equals(addr.getPlanCode()));

    // STREETCODE = STREETCODE
    public static BiPredicate<AddressRegistryBaseType, Addresses> streetCodeFilter = (addressRegistry, addr) ->
            planCodeFilter.test(addressRegistry, addr) &&
            (addressRegistry.getStreet() == null || addr.getStreetCode().equals(addressRegistry.getStreet().getCode()));


    // А_УУ_3 2.1.
    public static BiFunction<AddressRegistryBaseType, List<Addresses>, List<Addresses>> searchByStreetCode =
            (addressRegistry, addresses) -> {
                List<Addresses> outAddresses = addresses.stream().filter(addr ->
                        // AOLEVEL = 7
                        addr.getAoLevel().equals(AddressLevelType.STREET.getLevel())
                        && streetCodeFilter.test(addressRegistry, addr))
                        .collect(Collectors.toList());
                if (!outAddresses.isEmpty()) {
                    return outAddresses;
                } else {
                    return AlgorithmsHelper.checkPlanCodeExist.apply(addressRegistry, addresses);
                }
            };

    // А_УУ_3 2.2.
    public static BiFunction<AddressRegistryBaseType, List<Addresses>, List<Addresses>> searchByPlanCode =
            (addressRegistry, addresses) -> {
                List<Addresses> outAddresses = addresses.stream().filter(addr ->
                        planCodeFilter.test(addressRegistry, addr)
                        // AOLEVEL = 65
                        && addr.getAoLevel().equals(AddressLevelType.PLAN.getLevel()))
                        .collect(Collectors.toList());
                if (!outAddresses.isEmpty()) {
                    return outAddresses;
                } else {
                    return AlgorithmsHelper.checkPlaceCodeExist.apply(addressRegistry, addresses);
                }
            };

    // А_УУ_3 2.3.
    public static BiFunction<AddressRegistryBaseType, List<Addresses>, List<Addresses>> searchByPlaceCode =
            (addressRegistry, addresses) -> {
                List<Addresses> outAddresses = addresses.stream().filter(addr ->
                        placeCodeFilter.test(addressRegistry, addr)
                        // AOLEVEL = 6
                        && addr.getAoLevel().equals(AddressLevelType.PLACE.getLevel()))
                        .collect(Collectors.toList());
                if (!outAddresses.isEmpty()) {
                    return outAddresses;
                } else {
                    return AlgorithmsHelper.checkCityCodeExist.apply(addressRegistry, addresses);
                }
            };

    // А_УУ_3 2.4.
    public static BiFunction<AddressRegistryBaseType, List<Addresses>, List<Addresses>> searchByCityCode =
            (addressRegistry, addresses) -> {
                List<Addresses> outAddresses = addresses.stream().filter(addr ->
                        cityCodeFilter.test(addressRegistry, addr)
                        // AOLEVEL = 4
                        && addr.getAoLevel().equals(AddressLevelType.CITY.getLevel()))
                        .collect(Collectors.toList());
                if (!outAddresses.isEmpty()) {
                    return outAddresses;
                } else {
                    return AlgorithmsHelper.checkAreaCodeExist.apply(addressRegistry, addresses);
                }
            };

    // А_УУ_3 2.5.
    public static BiFunction<AddressRegistryBaseType, List<Addresses>, List<Addresses>> searchByAreaCode =
            (addressRegistry, addresses) -> {
                List<Addresses> outAddresses = addresses.stream().filter(addr ->
                        areaCodeFilter.test(addressRegistry, addr)
                        // AOLEVEL = 3
                        && addr.getAoLevel().equals(AddressLevelType.AREA.getLevel()))
                        .collect(Collectors.toList());
                if (!outAddresses.isEmpty()) {
                    return outAddresses;
                } else {
                    return AlgorithmsHelper.checkAreaOmkTeCodeExist.apply(addressRegistry, addresses);
                }
            };

    // А_УУ_3 2.6.
    public static BiFunction<AddressRegistryBaseType, List<Addresses>, List<Addresses>> searchByAreaOmkTeCode =
            (addressRegistry, addresses) -> {
                String[] codes = addressRegistry.getAreaOMKTE().getCode().split(ADDRESS_CODE_VALUES_SPLITTER);
                List<Addresses> outAddresses = addresses.stream().filter(
                        addr -> addr.getAoLevel().equals(AddressLevelType.AREA_TE.getLevel()) &&
                                Arrays.stream(codes).anyMatch(c -> addr.getAreaCodeOmkTe().equals(c)))
                        .collect(Collectors.toList());
                if (!outAddresses.isEmpty()) {
                    return outAddresses;
                } else {
                    if (addressRegistry.getRegionOMKTE() != null) {
                        String[] codes2 = addressRegistry.getRegionOMKTE().getCode().split(ADDRESS_CODE_VALUES_SPLITTER);
                        return addresses.stream().filter(addr ->
                                addr.getAoLevel().equals(AddressLevelType.REGION_TE.getLevel()) &&
                                        Arrays.stream(codes2).anyMatch(c -> addr.getRegionTeCode().equals(c)))
                                .collect(Collectors.toList());
                    } else {
                        return new ArrayList<>();
                    }
                }
            };

    // А_УУ_3 b.
    public static BiFunction<AddressRegistryBaseType, List<Addresses>, List<Addresses>> checkStreetCodeExist =
            (addressRegistry, addresses) -> {
                if (addressRegistry.getStreet() != null) {
                    return AlgorithmsHelper.searchByStreetCode.apply(addressRegistry, addresses);
                } else {
                    return AlgorithmsHelper.checkPlanCodeExist.apply(addressRegistry, addresses);
                }
            };

    // А_УУ_3 b.2.
    public static BiFunction<AddressRegistryBaseType, List<Addresses>, List<Addresses>> checkPlanCodeExist =
            (addressRegistry, addresses) -> {
                if (addressRegistry.getPlan() != null) {
                    return AlgorithmsHelper.searchByPlanCode.apply(addressRegistry, addresses);
                } else {
                    return AlgorithmsHelper.checkPlaceCodeExist.apply(addressRegistry, addresses);
                }
            };

    // А_УУ_3 b.2.2.
    public static BiFunction<AddressRegistryBaseType, List<Addresses>, List<Addresses>> checkPlaceCodeExist =
            (addressRegistry, addresses) -> {
                if (addressRegistry.getPlace() != null) {
                    return AlgorithmsHelper.searchByPlaceCode.apply(addressRegistry, addresses);
                } else {
                    return AlgorithmsHelper.checkCityCodeExist.apply(addressRegistry, addresses);
                }
            };

    // А_УУ_3 b.2.2.2.
    public static BiFunction<AddressRegistryBaseType, List<Addresses>, List<Addresses>> checkCityCodeExist =
            (addressRegistry, addresses) -> {
                if (addressRegistry.getCity() != null) {
                    return AlgorithmsHelper.searchByCityCode.apply(addressRegistry, addresses);
                } else {
                    return AlgorithmsHelper.checkAreaCodeExist.apply(addressRegistry, addresses);
                }
            };

    // А_УУ_3 b.2.2.2.2.
    public static BiFunction<AddressRegistryBaseType, List<Addresses>, List<Addresses>> checkAreaCodeExist =
            (addressRegistry, addresses) -> {
                if (addressRegistry.getArea() != null) {
                    return AlgorithmsHelper.searchByAreaCode.apply(addressRegistry, addresses);
                } else {
                    return AlgorithmsHelper.checkAreaOmkTeCodeExist.apply(addressRegistry, addresses);
                }
            };

    // А_УУ_3 b.2.2.2.2.2.
    public static BiFunction<AddressRegistryBaseType, List<Addresses>, List<Addresses>> checkAreaOmkTeCodeExist =
            (addressRegistry, addresses) -> {
                if (addressRegistry.getAreaOMKTE() != null) {
                    return AlgorithmsHelper.searchByAreaOmkTeCode.apply(addressRegistry, addresses);
                } else {
                    return null; //  С_УУ_105
//                    throw new ContingentException(AreaErrorReason.INCORRECT_ADDRESS_NESTING);
                }
            };
}
