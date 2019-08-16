package moscow.ptnl.contingent.area.transform;

import moscow.ptnl.contingent.area.entity.nsi.NsiAddressFormingElement;
import moscow.ptnl.contingent.area.entity.nsi.NsiBuildingRegistry;
import moscow.ptnl.contingent.area.model.area.AddressLevelType;
import org.springframework.stereotype.Component;
import ru.mos.emias.contingent2.core.Address;

@Component
public class AddressMapper implements Transform<Address, moscow.ptnl.contingent.area.entity.area.Addresses> {

    @Override
    public Address entityToDtoTransform(moscow.ptnl.contingent.area.entity.area.Addresses entityObject) {
        //TODO fix
//        NsiAddressFormingElement afe = AddressLevelType.ID.getLevel().equals(entityObject.getLevel()) ?
//                entityObject.getBuildingRegistry().getAddressFormingElement() :
//                entityObject.getAddressFormingElement();
//        NsiBuildingRegistry building = AddressLevelType.ID.getLevel().equals(entityObject.getLevel()) ?
//                entityObject.getBuildingRegistry() : null;
        Address address = new Address();
//        address.setLevelAddress(entityObject.getLevel());
//
//        if (afe != null) {
//            address.setAfeId(afe.getId());
//            address.setAfeGlobalId(afe.getGlobalId());
//            address.setRegionTeId(afe.getRegionTeId());
//            address.setRegionTeCode(afe.getRegionTeCode());
//            address.setRegionTeName(afe.getRegionTeName());
//            address.setRegionTeTypeName(afe.getRegionTeTypeName());
//            address.setAreaTeId(afe.getAreaTeId());
//            address.setAreaCodeOmkTe(afe.getAreaCodeOmkTe());
//            address.setAreaTeName(afe.getAreaTeName());
//            address.setAreaTeTypeName(afe.getAreaTeTypeName());
//            address.setDistrictId(afe.getAreaId());
//            address.setDistrictCode(afe.getAreaCode());
//            address.setDistrictName(afe.getAreaName());
//            address.setDistrictTypeName(afe.getAreaTypeName());
//            address.setCityId(afe.getCityId());
//            address.setCityCode(afe.getCityCode());
//            address.setCityName(afe.getCityName());
//            address.setCityTypeName(afe.getCityTypeName());
//            address.setPlaceId(afe.getPlaceId());
//            address.setPlaceCode(afe.getPlaceCode());
//            address.setPlaceName(afe.getPlaceName());
//            address.setPlaceTypeName(afe.getPlaceTypeName());
//            address.setPlanId(afe.getPlanId());
//            address.setPlanCode(afe.getPlanCode());
//            address.setPlanName(afe.getPlanName());
//            address.setPlanTypeName(afe.getPlanTypeName());
//            address.setStreetId(afe.getStreetId());
//            address.setStreetCode(afe.getStreetCode());
//            address.setStreetName(afe.getStreetName());
//            address.setStreetTypeName(afe.getStreetTypeName());
//        }
//        if (building != null) {
//            address.setBrId(building.getId());
//            address.setBrGlobalId(building.getGlobalId());
//            address.setBrAfeId(building.getAddressFormingElement().getId());
//            address.setHouseType(building.getL1Type());
//            address.setHouse(building.getL1Value());
//            address.setBuildingType(building.getL2Type());
//            address.setBuilding(building.getL2Value());
//            address.setConstructionType(building.getL3Type());
//            address.setConstruction(building.getL3Value());
//        }
        return address;
    }

    @Override
    public moscow.ptnl.contingent.area.entity.area.Addresses dtoToEntityTransform(Address dtoObject) {
        return null;
    }
}
