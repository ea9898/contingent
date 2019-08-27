package moscow.ptnl.contingent.area.service;

import moscow.ptnl.contingent.area.entity.nsi.NsiAddressFormingElement;
import moscow.ptnl.contingent.area.entity.nsi.NsiBuildingRegistry;
import moscow.ptnl.contingent.area.model.area.Address4Algoritm;
import moscow.ptnl.contingent.area.model.area.AddressLevelType;
import moscow.ptnl.contingent.area.model.area.AddressWrapper;
import moscow.ptnl.contingent.nsi.repository.AddressFormingElementCRUDRepository;
import moscow.ptnl.contingent.nsi.repository.AddressFormingElementRepository;
import moscow.ptnl.contingent.nsi.repository.BuildingRegistryCRUDRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.function.BiFunction;
import java.util.stream.Collectors;

@Component
public class AlgorithmsHelper {

    @Autowired
    AddressFormingElementRepository addressFormingElementRepository;

    @Autowired
    private AddressFormingElementCRUDRepository addressFormingElementCRUDRepository;

    @Autowired
    private BuildingRegistryCRUDRepository buildingRegistryCRUDRepository;

    // А_УУ_3 1.1.
    public static BiFunction<NsiAddressFormingElement, List<AddressWrapper>, List<AddressWrapper>> searchById =
            (addressFormingElement, addressWrappers) -> {
        List<AddressWrapper> outAddresses = addressWrappers.stream()
                .filter(aw -> aw.getBrGlobalId().equals(addressFormingElement.getGlobalId()))
                .collect(Collectors.toList());
        // a.
        if (!outAddresses.isEmpty()) {
            return outAddresses;
        } else {
            // b.
            return AlgorithmsHelper.checkStreetIdExist.apply(addressFormingElement, addressWrappers);
        }
    };

    // А_УУ_3 b.
    public static BiFunction<NsiAddressFormingElement, List<AddressWrapper>, List<AddressWrapper>> checkStreetIdExist =
            (addressFormingElement, addressWrappers) -> {
        //if (addressFormingElement.getCityId() != null) {
            return AlgorithmsHelper.crossByCity.apply(addressFormingElement, addressWrappers);
        //} else {
        //    return AlgorithmsHelper.checkPlanIdExist.apply(addressFormingElement, addressWrappers);
        //}
    };

    // А_УУ_3 1.2.
    public static BiFunction<NsiAddressFormingElement, List<AddressWrapper>, List<AddressWrapper>> crossByCity =
            (addressFormingElement, addressWrappers) -> {
        List<AddressWrapper> outAddresses = addressWrappers.stream()
                .filter(aw -> /*aw.getAddressFormingElement().getStreetId().equals(addressFormingElement.getStreetId())
                && */aw.getAddressFormingElement().getAoLevel().equals(String.valueOf(AddressLevelType.STREET.getLevel())))
                .collect(Collectors.toList());
        if (!outAddresses.isEmpty()) {
            return outAddresses;
        } else {
            return AlgorithmsHelper.checkPlanIdExist.apply(addressFormingElement, addressWrappers);
        }
    };

    // А_УУ_3 b.2.
    public static BiFunction<NsiAddressFormingElement, List<AddressWrapper>, List<AddressWrapper>> checkPlanIdExist =
            (addressFormingElement, addressWrappers) -> {
        /*if (addressFormingElement.getPlanId() != null) {*/
            return AlgorithmsHelper.crossByPlanId.apply(addressFormingElement, addressWrappers);
        /*} else {
            return AlgorithmsHelper.checkPlaceIdExist.apply(addressFormingElement, addressWrappers);
        }*/
    };

    // А_УУ_3 1.3.
    public static BiFunction<NsiAddressFormingElement, List<AddressWrapper>, List<AddressWrapper>> crossByPlanId =
            (addressFormingElement, addressWrappers) -> {
                List<AddressWrapper> outAddresses = addressWrappers.stream()
                        .filter(aw -> /*aw.getAddressFormingElement().getPlanId().equals(addressFormingElement.getPlanId())
                                &&*/ aw.getAddressFormingElement().getAoLevel().equals(String.valueOf(AddressLevelType.PLAN.getLevel())))
                        .collect(Collectors.toList());
                if (!outAddresses.isEmpty()) {
                    return outAddresses;
                } else {
                    return AlgorithmsHelper.checkPlaceIdExist.apply(addressFormingElement, addressWrappers);
                }
            };

    // А_УУ_3 b.2.2.
    public static BiFunction<NsiAddressFormingElement, List<AddressWrapper>, List<AddressWrapper>> checkPlaceIdExist =
            (addressFormingElement, addressWrappers) -> {
                /*if (addressFormingElement.getPlaceId() != null) {*/
                    return AlgorithmsHelper.crossByPlaceId.apply(addressFormingElement, addressWrappers);
                /*} else {
                    return AlgorithmsHelper.checkCityIdExist.apply(addressFormingElement, addressWrappers);
                }*/
            };

    // А_УУ_3 1.4.
    public static BiFunction<NsiAddressFormingElement, List<AddressWrapper>, List<AddressWrapper>> crossByPlaceId =
            (addressFormingElement, addressWrappers) -> {
                  List<AddressWrapper> outAddresses = addressWrappers.stream()
                          .filter(aw -> /*aw.getAddressFormingElement().getPlaceId().equals(addressFormingElement.getPlaceId())
                          && */aw.getAddressFormingElement().getAoLevel().equals(String.valueOf(AddressLevelType.PLACE.getLevel())))
                          .collect(Collectors.toList());
                  if (!outAddresses.isEmpty()) {
                      return outAddresses;
                  } else {
                      return AlgorithmsHelper.checkCityIdExist.apply(addressFormingElement, addressWrappers);
                  }
            };

    // А_УУ_3 b.2.2.2.
    public static BiFunction<NsiAddressFormingElement, List<AddressWrapper>, List<AddressWrapper>> checkCityIdExist =
        (addressFormingElement, addressWrappers) -> {
            /*if (addressFormingElement.getCityId() != null) {*/
                return AlgorithmsHelper.crossByCityId.apply(addressFormingElement, addressWrappers);
            /*} else {
                return AlgorithmsHelper.checkAreaIdExist.apply(addressFormingElement, addressWrappers);
            }*/
        };

    // А_УУ_3 1.5.
    public static BiFunction<NsiAddressFormingElement, List<AddressWrapper>, List<AddressWrapper>> crossByCityId =
            (addressFormingElement, addressWrappers) -> {
                List<AddressWrapper> outAddresses = addressWrappers.stream()
                        .filter(aw -> /*aw.getAddressFormingElement().getCityId().equals(addressFormingElement.getCityId())
                                && */aw.getAddressFormingElement().getAoLevel().equals(String.valueOf(AddressLevelType.CITY.getLevel())))
                        .collect(Collectors.toList());
                if (!outAddresses.isEmpty()) {
                    return outAddresses;
                } else {
                    return AlgorithmsHelper.checkAreaIdExist.apply(addressFormingElement, addressWrappers);
                }
            };

    // А_УУ_3 b.2.2.2.2.
    public static BiFunction<NsiAddressFormingElement, List<AddressWrapper>, List<AddressWrapper>> checkAreaIdExist =
            (addressFormingElement, addressWrappers) -> {
                /*if (addressFormingElement.getAreaId() != null) {*/
                    return AlgorithmsHelper.crossByAreaId.apply(addressFormingElement, addressWrappers);
                /*} else {
                    return AlgorithmsHelper.checkAreaTeIdExist.apply(addressFormingElement, addressWrappers);
                }*/
            };

    // А_УУ_3 1.6.
    public static BiFunction<NsiAddressFormingElement, List<AddressWrapper>, List<AddressWrapper>> crossByAreaId =
            (addressFormingElement, addressWrappers) -> {
                List<AddressWrapper> outAddresses = addressWrappers.stream()
                        .filter(aw -> /*aw.getAddressFormingElement().getAreaId().equals(addressFormingElement.getAreaId())
                                && */aw.getAddressFormingElement().getAoLevel().equals(String.valueOf(AddressLevelType.AREA.getLevel())))
                        .collect(Collectors.toList());
                if (!outAddresses.isEmpty()) {
                    return outAddresses;
                } else {
                    return AlgorithmsHelper.checkAreaTeIdExist.apply(addressFormingElement, addressWrappers);
                }
            };


    // А_УУ_3 b.2.2.2.2.2.
    public static BiFunction<NsiAddressFormingElement, List<AddressWrapper>, List<AddressWrapper>> checkAreaTeIdExist =
            (addressFormingElement, addressWrappers) -> {
                /*if (addressFormingElement.getAreaTeId() != null) {*/
                    return AlgorithmsHelper.crossByAreaTeId.apply(addressFormingElement, addressWrappers);
                /*} else {
                    return null;
                }*/
            };


    // А_УУ_3 1.7.
    public static BiFunction<NsiAddressFormingElement, List<AddressWrapper>, List<AddressWrapper>> crossByAreaTeId =
            (addressFormingElement, addressWrappers) ->
                addressWrappers.stream()
                        .filter(aw -> /*aw.getAddressFormingElement().getAreaTeId().equals(addressFormingElement.getAreaTeId())
                                && */aw.getAddressFormingElement().getAoLevel().equals(String.valueOf(AddressLevelType.AREA_TE.getLevel())))
                        .collect(Collectors.toList());

    // А_УУ_3 2.
    public static BiFunction<NsiAddressFormingElement, List<AddressWrapper>, List<AddressWrapper>> searchByStreetId =
            (addressFormingElement, addressWrappers) -> {
                List<AddressWrapper> outAddresses = addressWrappers/*.stream()
                        .filter(aw -> aw.getAddressFormingElement().getStreetId().equals(addressFormingElement.getGlobalId()))
                            .collect(Collectors.toList())*/;
                if (!outAddresses.isEmpty()) {
                    return outAddresses;
                } else {
                    return AlgorithmsHelper.checkPlanIdExist.apply(addressFormingElement, addressWrappers);
                }
            };

    // А_УУ_3 3.
    public static BiFunction<NsiAddressFormingElement, List<AddressWrapper>, List<AddressWrapper>> searchByPlanId =
            (addressFormingElement, addressWrappers) -> {
                List<AddressWrapper> outAddresses = addressWrappers/*.stream()
                        .filter(aw -> aw.getAddressFormingElement().getPlanId().equals(addressFormingElement.getGlobalId()))
                        .collect(Collectors.toList())*/;
                if (!outAddresses.isEmpty()) {
                    return outAddresses;
                } else {
                    return AlgorithmsHelper.checkPlaceIdExist.apply(addressFormingElement, addressWrappers);
                }
            };


    // А_УУ_3 4.
    public static BiFunction<NsiAddressFormingElement, List<AddressWrapper>, List<AddressWrapper>> searchByPlaceId =
            (addressFormingElement, addressWrappers) -> {
                List<AddressWrapper> outAddresses = addressWrappers/*.stream()
                        .filter(aw -> aw.getAddressFormingElement().getPlaceId().equals(addressFormingElement.getGlobalId()))
                        .collect(Collectors.toList())*/;
                if (!outAddresses.isEmpty()) {
                    return outAddresses;
                } else {
                    return AlgorithmsHelper.checkCityIdExist.apply(addressFormingElement, addressWrappers);
                }
            };

    // А_УУ_3 5.
    public static BiFunction<NsiAddressFormingElement, List<AddressWrapper>, List<AddressWrapper>> searchByCityId =
            (addressFormingElement, addressWrappers) -> {
                List<AddressWrapper> outAddresses = addressWrappers/*.stream()
                        .filter(aw -> aw.getAddressFormingElement().getCityId().equals(addressFormingElement.getGlobalId()))
                        .collect(Collectors.toList())*/;
                if (!outAddresses.isEmpty()) {
                    return outAddresses;
                } else {
                    return AlgorithmsHelper.checkAreaIdExist.apply(addressFormingElement, addressWrappers);
                }
            };

    // А_УУ_3 6.
    public static BiFunction<NsiAddressFormingElement, List<AddressWrapper>, List<AddressWrapper>> searchByAreaId =
            (addressFormingElement, addressWrappers) -> {
                List<AddressWrapper> outAddresses = addressWrappers/*.stream()
                        .filter(aw -> aw.getAddressFormingElement().getAreaId().equals(addressFormingElement.getGlobalId()))
                        .collect(Collectors.toList())*/;
                if (!outAddresses.isEmpty()) {
                    return outAddresses;
                } else {
                    return AlgorithmsHelper.checkAreaTeIdExist.apply(addressFormingElement, addressWrappers);
                }
            };

    // А_УУ_3 7.
    public static BiFunction<NsiAddressFormingElement, List<AddressWrapper>, List<AddressWrapper>> searchByAreaTeId =
            (addressFormingElement, addressWrappers) -> {
                List<AddressWrapper> outAddresses = addressWrappers/*.stream()
                        .filter(aw -> aw.getAddressFormingElement().getAreaTeId().equals(addressFormingElement.getGlobalId()))
                        .collect(Collectors.toList())*/;
                if (!outAddresses.isEmpty()) {
                    return outAddresses;
                } else {
                    return addressWrappers.stream()
                            .filter(aw -> /*aw.getAddressFormingElement().getRegionTeId().equals(addressFormingElement.getRegionTeId())
                            && */aw.getAddressFormingElement().getAoLevel().equals(String.valueOf(AddressLevelType.REGION_TE.getLevel())))
                            .collect(Collectors.toList());

                }
            };

    // А_УУ_1, А_УУ_2 3.
    public List<AddressWrapper> createAfeBrList(List<Address4Algoritm> addresses4Algoritm) {
        List<AddressWrapper> addressWrappers = new ArrayList<>();
        addresses4Algoritm.forEach(al -> {
            if (!AddressLevelType.ID.getLevel().equals(al.getLevel())) {
                // 3.1.
                List<NsiAddressFormingElement> addressFormingElements = addressFormingElementRepository.findAfeByIdAndLevel(
                        al.getAddressId(), al.getLevel());
                if (!addressFormingElements.isEmpty()) {
                    AddressWrapper addressWrapper = new AddressWrapper(addressFormingElements.get(0));
                    addressWrapper.setAddress(al.getAddresses());
                    addressWrappers.add(addressWrapper);
                }
            } else {
                // 3.2.
                // a.
                Optional<NsiBuildingRegistry> buildingRegistryOptional = buildingRegistryCRUDRepository.findById(al.getAddressId());
                if (buildingRegistryOptional.isPresent()) {
                    NsiBuildingRegistry buildingRegistry = buildingRegistryOptional.get();
                    Long afeId = buildingRegistry.getAddressFormingElement().getId();

                    // b.
                    NsiAddressFormingElement addressFormingElement = addressFormingElementCRUDRepository.findById(afeId).get();

                    // c.
                    AddressWrapper addressWrapper = new AddressWrapper();
                    addressWrapper.setAddressFormingElement(addressFormingElement);
                    addressWrapper.setBuildingRegistry(buildingRegistry);

                    // d.
                    addressWrapper.setAddressLevelType(AddressLevelType.ID);
                    addressWrapper.setBrGlobalId(buildingRegistry.getGlobalId());

                    addressWrapper.setAddress(al.getAddresses());

                    addressWrappers.add(addressWrapper);
                }

            }
        });
        return addressWrappers;
    }
}
