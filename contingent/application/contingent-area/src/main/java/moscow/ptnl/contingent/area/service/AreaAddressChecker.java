package moscow.ptnl.contingent.area.service;

import moscow.ptnl.contingent.domain.area.entity.AreaAddress;
import moscow.ptnl.contingent.nsi.domain.area.NsiAddressFormingElement;
import moscow.ptnl.contingent.domain.AreaErrorReason;
import moscow.ptnl.contingent.error.Validation;
import moscow.ptnl.contingent.error.ValidationParameter;
import moscow.ptnl.contingent.domain.area.model.area.AddressLevelType;
import moscow.ptnl.contingent.domain.area.model.area.AddressWrapper;
import moscow.ptnl.contingent.domain.area.repository.AreaAddressRepository;
import moscow.ptnl.contingent.nsi.domain.repository.AddressFormingElementRepository;
import moscow.ptnl.contingent.nsi.repository.BuildingRegistryRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.*;
import java.util.stream.Collectors;

@Component
@Deprecated
public class AreaAddressChecker {

    @Autowired
    private BuildingRegistryRepository buildingRegistryRepository;

    @Autowired
    private AddressFormingElementRepository addressFormingElementRepository;

    @Autowired
    private AreaAddressRepository areaAddressRepository;

    @Deprecated
    public void checkMoAddressesExist(long moId, long areaTypeCode, List<AddressWrapper> newAddresses, Validation validation) {
        List<AddressWrapper> existingAddresses = new ArrayList<>();

        areaAddressRepository.getActiveAreaAddresses(moId, areaTypeCode).stream()
                .map(AreaAddress::getMoAddress)
                .forEach(a -> {
            AddressWrapper wrapper = new AddressWrapper();
            wrapper.moAddress = a;
            wrapper.address = a.getAddress();
            //TODO fix
//            if (AddressLevelType.ID.getLevel().equals(a.getAddress().getLevel())) {
//                wrapper.buildingRegistry = a.getAddress().getBuildingRegistry();
//                wrapper.addressFormingElement = a.getAddress().getBuildingRegistry().getAddressFormingElement();
//            }
//            else {
//                wrapper.addressFormingElement = a.getAddress().getAddressFormingElement();
//            }
            existingAddresses.add(wrapper);
        });
        Map<AddressWrapper, List<AddressWrapper>> foundAddresses = findCrossedNsiAddresses(existingAddresses, newAddresses);

        foundAddresses.forEach((key, value) -> {
            if (value.isEmpty() /*&& key.addressFormingElement.getAreaTeId() == null*/) {
                validation.error(AreaErrorReason.INCORRECT_ADDRESS_NESTING);
            }
            else if (!value.isEmpty()) {
                //Если пересекающиеся адреса найдены (алгоритм вернул ИД территории обслуживания МО),
                // то Система получает ИД МО и возвращает ошибку
                String moIdFound = String.join(", ", value.stream()
                        .map(a -> String.valueOf(a.moAddress.getMoId())).collect(Collectors.toSet()));
                String areaTypeIdFound = String.join(", ", value.stream()
                        .filter(a -> a.moAddress.getAreaType() != null)
                        .map(a -> String.valueOf(a.moAddress.getAreaType().getCode())).collect(Collectors.toSet()));

                if (key.nsiAddress != null) {
                    validation.error(AreaErrorReason.ADDRESS_ALREADY_EXISTS,
                            new ValidationParameter("levelAddress", key.nsiAddress.getLevelAddress()),
                            new ValidationParameter("globalId", key.nsiAddress.getGlobalId()),
                            new ValidationParameter("moId", moIdFound),
                            new ValidationParameter("areaTypeCode", areaTypeIdFound));
                }
            }
        });
    }

    private Map<AddressWrapper, List<AddressWrapper>> findCrossedNsiAddresses(
            List<AddressWrapper> existingAddresses, List<AddressWrapper> newAddresses) {
        Set<AddressLevelType> simpleCheck = new HashSet<>();
        Collections.addAll(simpleCheck, AddressLevelType.STREET, AddressLevelType.PLAN, AddressLevelType.PLACE,
                AddressLevelType.CITY, AddressLevelType.AREA, AddressLevelType.AREA_TE);

        Map<AddressWrapper, List<AddressWrapper>> result = new HashMap<>();
        //По адресам по справочнику
        newAddresses.stream()
                .filter(a -> a.nsiAddress != null)
                .forEach(a -> {
                    List<AddressWrapper> found = new ArrayList<>();

                    if (Objects.equals(a.nsiAddress.getLevelAddress(), AddressLevelType.ID.getLevel())) {
                        found = existingAddresses.stream()
                                .filter(b -> b.buildingRegistry != null)
                                .filter(b -> Objects.equals(b.buildingRegistry.getGlobalId(), a.nsiAddress.getGlobalId()))
                                .map(AddressWrapper::new)
                                .collect(Collectors.toList());

                        if (found.isEmpty()) {
                            found = findCrossedAddressesByFields(existingAddresses, null,
                                    AddressLevelType.find(a.nsiAddress.getLevelAddress()),
                                    a.buildingRegistry.getAddressFormingElement());
                        }
                    }
                    else if (simpleCheck.contains(AddressLevelType.find(a.nsiAddress.getLevelAddress()))) {
                        found = findCrossedAddressesByFields(existingAddresses, a.nsiAddress.getGlobalId(),
                                AddressLevelType.find(a.nsiAddress.getLevelAddress()),
                                a.addressFormingElement);
                    }
                    result.put(a, found);
                });

        return result;
    }

    private List<AddressWrapper> findCrossedAddressesByFields(List<AddressWrapper> existingAddresses, Long globalId,
                                                              AddressLevelType level, NsiAddressFormingElement afe) {
        //Ищем адреса в соответствии с их уровнем и заполненностью полей искомого адреса
        Map<AddressLevelType, List<AddressWrapper>> found = new HashMap<>();
        Arrays.stream(AddressLevelType.values())
                .forEach(l -> found.put(l, new ArrayList<>()));

        existingAddresses.stream()
                .filter(e -> e.addressFormingElement != null)
                .forEach(e -> {
                    //TODO fix
//                    if (afe.getStreetId() != null &&
//                            level.ordinal() < AddressLevelType.STREET.ordinal() &&
//                            afe.getStreetId().equals(e.addressFormingElement.getStreetId()) &&
//                            AddressLevelType.STREET.getLevel().equals(e.address.getLevel()) ||
//                            AddressLevelType.STREET.equals(level) &&
//                                    globalId != null &&
//                                    Objects.equals(globalId, e.addressFormingElement.getStreetId())) {
//                        found.get(AddressLevelType.STREET).add(new AddressWrapper(e));
//                    }
//                    if (afe.getPlanId() != null &&
//                            level.ordinal() < AddressLevelType.PLAN.ordinal() &&
//                            afe.getPlanId().equals(e.addressFormingElement.getPlanId()) &&
//                            AddressLevelType.PLAN.getLevel().equals(e.address.getLevel()) ||
//                            AddressLevelType.PLAN.equals(level) &&
//                                    globalId != null &&
//                                    Objects.equals(globalId, e.addressFormingElement.getPlanId())) {
//                        found.get(AddressLevelType.PLAN).add(new AddressWrapper(e));
//                    }
//                    if (afe.getPlaceId() != null &&
//                            level.ordinal() < AddressLevelType.PLACE.ordinal() &&
//                            afe.getPlaceId().equals(e.addressFormingElement.getPlaceId()) &&
//                            AddressLevelType.PLACE.getLevel().equals(e.address.getLevel()) ||
//                            AddressLevelType.PLACE.equals(level) &&
//                                    globalId != null &&
//                                    Objects.equals(globalId, e.addressFormingElement.getPlaceId())) {
//                        found.get(AddressLevelType.PLACE).add(new AddressWrapper(e));
//                    }
//                    if (afe.getCityId() != null &&
//                            level.ordinal() < AddressLevelType.CITY.ordinal() &&
//                            afe.getCityId().equals(e.addressFormingElement.getCityId()) &&
//                            AddressLevelType.CITY.getLevel().equals(e.address.getLevel()) ||
//                            AddressLevelType.CITY.equals(level) &&
//                                    globalId != null &&
//                                    Objects.equals(globalId, e.addressFormingElement.getCityId())) {
//                        found.get(AddressLevelType.CITY).add(new AddressWrapper(e));
//                    }
//                    if (afe.getAreaId() != null &&
//                            level.ordinal() < AddressLevelType.AREA.ordinal() &&
//                            afe.getAreaId().equals(e.addressFormingElement.getAreaId()) &&
//                            AddressLevelType.AREA.getLevel().equals(e.address.getLevel()) ||
//                            AddressLevelType.AREA.equals(level) &&
//                                    globalId != null &&
//                                    Objects.equals(globalId, e.addressFormingElement.getAreaId())) {
//                        found.get(AddressLevelType.AREA).add(new AddressWrapper(e));
//                    }
//                    if (afe.getAreaTeId() != null &&
//                            level.ordinal() < AddressLevelType.AREA_TE.ordinal() &&
//                            afe.getAreaTeId().equals(e.addressFormingElement.getAreaTeId()) &&
//                            AddressLevelType.AREA_TE.getLevel().equals(e.address.getLevel()) ||
//                            AddressLevelType.AREA_TE.equals(level) &&
//                                    globalId != null &&
//                                    Objects.equals(globalId, e.addressFormingElement.getAreaTeId())) {
//                        found.get(AddressLevelType.AREA_TE).add(new AddressWrapper(e));
//                    }
//                    if (afe.getRegionTeId() != null &&
//                            AddressLevelType.AREA_TE.equals(level) &&
//                            afe.getRegionTeId().equals(e.addressFormingElement.getRegionTeId()) &&
//                            AddressLevelType.REGION_TE.getLevel().equals(e.address.getLevel())) {
//                        found.get(AddressLevelType.REGION_TE).add(new AddressWrapper(e));
//                    }
                });
        for (AddressLevelType addressLevelType : AddressLevelType.values()) {
            if (!found.get(addressLevelType).isEmpty()) {
                return found.get(addressLevelType);
            }
        }
        return Collections.EMPTY_LIST;
    }
}
