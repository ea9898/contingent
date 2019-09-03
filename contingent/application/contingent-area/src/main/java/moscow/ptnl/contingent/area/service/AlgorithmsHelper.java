package moscow.ptnl.contingent.area.service;

import moscow.ptnl.contingent.area.entity.area.Addresses;
import moscow.ptnl.contingent.area.entity.nsi.NsiAddressFormingElement;
import moscow.ptnl.contingent.area.entity.nsi.NsiBuildingRegistry;
import moscow.ptnl.contingent.area.error.AreaErrorReason;
import moscow.ptnl.contingent.area.error.ContingentException;
import moscow.ptnl.contingent.area.model.area.Address4Algoritm;
import moscow.ptnl.contingent.area.model.area.AddressLevelType;
import moscow.ptnl.contingent.area.model.area.AddressWrapper;
import moscow.ptnl.contingent.nsi.repository.AddressFormingElementCRUDRepository;
import moscow.ptnl.contingent.nsi.repository.AddressFormingElementRepository;
import moscow.ptnl.contingent.nsi.repository.BuildingRegistryCRUDRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import ru.mos.emias.contingent2.address.AddressRegistryBaseType;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.function.BiFunction;
import java.util.stream.Collectors;

@Component
public class AlgorithmsHelper {

    // А_УУ_3 2.1.
    public static BiFunction<AddressRegistryBaseType, List<Addresses>, List<Addresses>> searchByStreetCode =
            (addressRegistry, addresses) -> {
                List<Addresses> outAddresses = addresses.stream().filter(addr -> addr.getAoLevel()
                        .equals(AddressLevelType.STREET.getLevel()) && addr.getStreetCode().equals(addressRegistry.getStreet().getCode()))
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
                List<Addresses> outAddresses = addresses.stream().filter(addr -> addr.getAoLevel()
                        .equals(AddressLevelType.PLAN.getLevel()) && addr.getPlanCode().equals(addressRegistry.getPlan().getCode()))
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
                List<Addresses> outAddresses = addresses.stream().filter(addr -> addr.getAoLevel()
                        .equals(AddressLevelType.PLACE.getLevel()) && addr.getPlaceCode().equals(addressRegistry.getPlace().getCode()))
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
                List<Addresses> outAddresses = addresses.stream().filter(addr -> addr.getAoLevel()
                        .equals(AddressLevelType.CITY.getLevel()) && addr.getCityCode().equals(addressRegistry.getCity().getCode()))
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
                List<Addresses> outAddresses = addresses.stream().filter(addr -> addr.getAoLevel()
                        .equals(AddressLevelType.AREA.getLevel()) && addr.getAreaCode().equals(addressRegistry.getArea().getCode()))
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
                List<Addresses> outAddresses = addresses.stream().filter(addr -> addr.getAoLevel()
                        .equals(AddressLevelType.AREA_TE.getLevel()) && addr.getAreaCode().equals(addressRegistry.getAreaOMKTE().getCode()))
                        .collect(Collectors.toList());
                if (!outAddresses.isEmpty()) {
                    return outAddresses;
                } else {
                    return addresses.stream().filter(addr -> addr.getAoLevel()
                            .equals(AddressLevelType.REGION_TE.getLevel()) && addr.getAreaCode().equals(addressRegistry.getRegionOMKTE().getCode()))
                            .collect(Collectors.toList());
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
