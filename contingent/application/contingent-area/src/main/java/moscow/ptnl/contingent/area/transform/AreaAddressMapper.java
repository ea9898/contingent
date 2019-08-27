package moscow.ptnl.contingent.area.transform;

import moscow.ptnl.contingent.area.entity.nsi.NsiBuildingRegistry;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import ru.mos.emias.contingent2.core.Address;
import ru.mos.emias.contingent2.core.AreaAddress;

@Component
public class AreaAddressMapper implements Transform<AreaAddress, moscow.ptnl.contingent.area.model.area.AddressArea> {

    @Autowired
    private AddressMapper addressMapper;

    @Override
    public AreaAddress entityToDtoTransform(moscow.ptnl.contingent.area.model.area.AddressArea entityObject) {
        AreaAddress areaAddress = new AreaAddress();
        areaAddress.setAreaAddressId(entityObject.getAreaAddressId());

        Address address = new Address();

        address.setAfeId(entityObject.getAddressFormingElement().getId());
        // TODO  Странная ситуация...
        address.setLevelAddress(Integer.parseInt(entityObject.getAddressFormingElement().getAoLevel()));
        address.setAfeGlobalId(entityObject.getAddressFormingElement().getGlobalId());

        //address.setRegionTeId(entityObject.getAddressFormingElement().getRegionTeId());
        address.setRegionTeCode(entityObject.getAddressFormingElement().getRegionTeCode());
        address.setRegionTeName(entityObject.getAddressFormingElement().getRegionTeName());
        address.setRegionTeTypeName(entityObject.getAddressFormingElement().getRegionTeTypeName());

        //address.setAreaTeId(entityObject.getAddressFormingElement().getAreaTeId());
        address.setAreaCodeOmkTe(entityObject.getAddressFormingElement().getAreaCodeOmkTe());
        address.setAreaTeName(entityObject.getAddressFormingElement().getAreaTeName());
        address.setAreaTeTypeName(entityObject.getAddressFormingElement().getAreaTeTypeName());

        //address.setDistrictId(entityObject.getAddressFormingElement().getAreaId());
        address.setDistrictCode(entityObject.getAddressFormingElement().getAreaCode());
        address.setDistrictName(entityObject.getAddressFormingElement().getAreaName());
        address.setDistrictTypeName(entityObject.getAddressFormingElement().getAreaTypeName());

        //address.setCityId(entityObject.getAddressFormingElement().getCityId());
        address.setCityCode(entityObject.getAddressFormingElement().getCityCode());
        address.setCityName(entityObject.getAddressFormingElement().getCityName());
        address.setCityTypeName(entityObject.getAddressFormingElement().getCityTypeName());

        //address.setPlaceId(entityObject.getAddressFormingElement().getPlaceId());
        address.setPlaceCode(entityObject.getAddressFormingElement().getPlaceCode());
        address.setPlaceName(entityObject.getAddressFormingElement().getPlaceName());
        address.setPlaceTypeName(entityObject.getAddressFormingElement().getPlaceTypeName());

        //address.setPlanId(entityObject.getAddressFormingElement().getPlanId());
        address.setPlanCode(entityObject.getAddressFormingElement().getPlanCode());
        address.setPlanName(entityObject.getAddressFormingElement().getPlanName());
        address.setPlanTypeName(entityObject.getAddressFormingElement().getPlanTypeName());

        //address.setStreetId(entityObject.getAddressFormingElement().getStreetId());
        address.setStreetCode(entityObject.getAddressFormingElement().getStreetCode());
        address.setStreetName(entityObject.getAddressFormingElement().getStreetName());
        address.setStreetTypeName(entityObject.getAddressFormingElement().getStreetTypeName());

        if (entityObject.getBuildingRegistry() != null) {
            NsiBuildingRegistry buildingRegistry = entityObject.getBuildingRegistry();
            address.setBrId(buildingRegistry.getId());
            address.setBrGlobalId(buildingRegistry.getGlobalId());
            address.setBrAfeId(buildingRegistry.getAddressFormingElement().getId());
            address.setHouseType(buildingRegistry.getL1Type());
            address.setHouse(buildingRegistry.getL1Value());
            address.setBuildingType(buildingRegistry.getL2Type());
            address.setBuilding(buildingRegistry.getL2Value());
            address.setConstructionType(buildingRegistry.getL3Type());
            address.setConstruction(buildingRegistry.getL3Value());
        }

        areaAddress.setAddress(address);
        return areaAddress;
    }

    @Override
    public moscow.ptnl.contingent.area.model.area.AddressArea dtoToEntityTransform(AreaAddress dtoObject) {
        return null;
    }
}
