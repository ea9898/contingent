package moscow.ptnl.contingent.area.transform.model.esu;

import moscow.ptnl.contingent.area.entity.area.AreaAddress;
import moscow.ptnl.contingent.area.model.area.AddressLevelType;
import moscow.ptnl.contingent.area.transform.Transform;
import moscow.ptnl.contingent2.area.info.Address;
import moscow.ptnl.contingent2.area.info.AreaInfoEvent;
import org.springframework.stereotype.Component;

import java.util.Set;

@Component
public class AddressesMapper implements Transform<AreaInfoEvent.Addresses, Set<AreaAddress>> {

    @Override
    public AreaInfoEvent.Addresses entityToDtoTransform(Set<AreaAddress> entity) {
        AreaInfoEvent.Addresses addresses = new AreaInfoEvent.Addresses();
        entity.stream()
                .map(AreaAddress::getAddress)
                .filter(e -> AddressLevelType.REGION_TE.getLevel().equals(e.getAoLevel())
                        || AddressLevelType.ID.getLevel().equals(e.getAoLevel())
                        || AddressLevelType.AREA_TE.getLevel().equals(e.getAoLevel())
                        || AddressLevelType.AREA.getLevel().equals(e.getAoLevel())
                            && e.getAreaBtiCode() != null
                        || AddressLevelType.CITY.getLevel().equals(e.getAoLevel())
                            && e.getCityBtiCode() != null
                        || AddressLevelType.PLACE.getLevel().equals(e.getAoLevel())
                            && e.getPlaceBtiCode() != null
                        || AddressLevelType.STREET.getLevel().equals(e.getAoLevel())
                            && (e.getStreetBtiCode() != null || e.getStreetOmkUm() != null)
                        || AddressLevelType.PLAN.getLevel().equals(e.getAoLevel())
                            && e.getPlanBtiCode() != null
                )
                .forEach(e -> {
                    Address address = new Address();
                    address.setAolevel(e.getAoLevel());
                    address.setOmkTeDistrictCode(e.getRegionTeCode());
                    if (!AddressLevelType.REGION_TE.getLevel().equals(e.getAoLevel())) {
                        address.setOmkTeRegionCode(e.getAreaCodeOmkTe());
                    }
                    if (AddressLevelType.AREA.getLevel().equals(e.getAoLevel())) {
                        address.setStreetBTICode(e.getAreaBtiCode());
                    }
                    if (AddressLevelType.CITY.getLevel().equals(e.getAoLevel())) {
                        address.setStreetBTICode(e.getCityBtiCode());
                    }
                    if (AddressLevelType.PLACE.getLevel().equals(e.getAoLevel())) {
                        address.setStreetBTICode(e.getPlaceBtiCode());
                    }
                    if (AddressLevelType.STREET.getLevel().equals(e.getAoLevel())) {
                        address.setOmkUmCode(e.getStreetOmkUm());
                        address.setStreetBTICode(e.getStreetBtiCode());
                    }
                    if (AddressLevelType.ID.getLevel().equals(e.getAoLevel())) {
                        address.setOmkUmCode(e.getStreetOmkUm());
                        address.setHouse(e.getL1Value());
                        address.setBuilding(e.getL2Value());
                        address.setConstruction(e.getL3Value());
                        if (e.getStreetBtiCode() != null) {
                            address.setStreetBTICode(e.getStreetBtiCode());
                        }else if (e.getPlanBtiCode() != null) {
                            address.setStreetBTICode(e.getPlanBtiCode());
                        }else if (e.getPlaceBtiCode() != null) {
                            address.setStreetBTICode(e.getPlaceBtiCode());
                        }else if (e.getCityBtiCode() != null) {
                            address.setStreetBTICode(e.getCityBtiCode());
                        }else if (e.getAreaBtiCode() != null) {
                            address.setStreetBTICode(e.getAreaBtiCode());
                        }
                    }
                    if (AddressLevelType.PLAN.getLevel().equals(e.getAoLevel())) {
                        address.setStreetBTICode(e.getPlanBtiCode());
                    }
                    addresses.getAddress().add(address);
                });
        return addresses;
    }

    @Override
    public Set<AreaAddress> dtoToEntityTransform(AreaInfoEvent.Addresses dtoObject) {
        return null;
    }
}
