package moscow.ptnl.contingent.area.transform.model.esu;

import moscow.ptnl.contingent.area.entity.area.AreaAddress;
import moscow.ptnl.contingent.area.entity.nsi.NsiAddressFormingElement;
import moscow.ptnl.contingent.area.model.area.AddressLevelType;
import moscow.ptnl.contingent.area.transform.Transform;
import moscow.ptnl.contingent2.area.info.Address;
import moscow.ptnl.contingent2.area.info.AreaInfoEvent;
import org.springframework.stereotype.Component;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

@Component
public class AddressesMapper implements Transform<AreaInfoEvent.Addresses, Set<AreaAddress>> {

    private static final Set<Integer> ADDRESS_LEVELS = new HashSet<>(Arrays.asList(2, 25, 7, 8));

    @Override
    public AreaInfoEvent.Addresses entityToDtoTransform(Set<AreaAddress> entity) {
        AreaInfoEvent.Addresses addresses = new AreaInfoEvent.Addresses();
        //TODO fix
//        entity.stream()
//                .map(AreaAddress::getAddress)
//                .filter(e -> ADDRESS_LEVELS.contains(e.getLevel()))
//                .forEach(e -> {
//            if (AddressLevelType.ID.getLevel().equals(e.getLevel())
//                    && e.getBuildingRegistry().getAddressFormingElement().getStreetOmkUm() != null) {
//                NsiAddressFormingElement afe = e.getBuildingRegistry().getAddressFormingElement();
//                Address address = new Address();
//                address.setOmkTeDistrictCode(afe.getRegionTeCode());
//                address.setOmkTeRegionCode(afe.getAreaCodeOmkTe());
//                address.setOmkUmCode(afe.getStreetOmkUm());
//                address.setHouse(e.getBuildingRegistry().getL1Value());
//                address.setBuilding(e.getBuildingRegistry().getL2Value());
//                address.setConstruction(e.getBuildingRegistry().getL3Value());
//                addresses.getAddress().add(address);
//            }
//            else if (AddressLevelType.STREET.getLevel().equals(e.getLevel()) && e.getAddressFormingElement().getStreetOmkUm() != null
//                    || AddressLevelType.AREA_TE.getLevel().equals(e.getLevel()) && e.getAddressFormingElement().getAreaCodeOmkTe() != null
//                    || AddressLevelType.REGION_TE.getLevel().equals(e.getLevel()) && e.getAddressFormingElement().getRegionTeCode() != null) {
//                Address address = new Address();
//                address.setOmkTeDistrictCode(e.getAddressFormingElement().getRegionTeCode());
//                address.setOmkTeRegionCode(AddressLevelType.REGION_TE.getLevel().equals(e.getLevel())
//                        ? null : e.getAddressFormingElement().getAreaCodeOmkTe());
//                address.setOmkUmCode(AddressLevelType.STREET.getLevel().equals(e.getLevel())
//                        ? e.getAddressFormingElement().getStreetOmkUm() : null);
//                addresses.getAddress().add(address);
//            }
//        });
        return addresses;
    }

    @Override
    public Set<AreaAddress> dtoToEntityTransform(AreaInfoEvent.Addresses dtoObject) {
        return null;
    }
}
