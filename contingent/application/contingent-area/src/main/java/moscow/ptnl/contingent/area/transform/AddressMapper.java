package moscow.ptnl.contingent.area.transform;

import moscow.ptnl.contingent.area.entity.area.Addresses;
import moscow.ptnl.contingent.area.entity.nsi.NsiAddressFormingElement;
import moscow.ptnl.contingent.area.entity.nsi.NsiBuildingRegistry;
import moscow.ptnl.contingent.area.model.area.AddressLevelType;
import org.springframework.stereotype.Component;
import ru.mos.emias.contingent2.address.AddressRegistryBaseType;
import ru.mos.emias.contingent2.core.Address;

@Component
public class AddressMapper implements Transform<AddressRegistryBaseType, moscow.ptnl.contingent.area.entity.area.Addresses> {

    @Override
    public AddressRegistryBaseType entityToDtoTransform(Addresses entityObject) {
        return null;
    }

    @Override
    public Addresses dtoToEntityTransform(AddressRegistryBaseType dtoObject) {
        Addresses addresses = new Addresses();
        addresses.setGlobalId(dtoObject.getGlobalIdNsi());
        addresses.setAoLevel(dtoObject.getAoLevel());
        addresses.setAddress(dtoObject.getAddressString());

        addresses.setRegionTeCode(dtoObject.getRegionOMKTE().getCode());
        addresses.setRegionTeName(dtoObject.getRegionOMKTE().getName());
        addresses.setRegionTeTypeName(dtoObject.getRegionOMKTE().getType().getFull());
        addresses.setRegionTeTypeNameShort(dtoObject.getRegionOMKTE().getType().getShort());

        addresses.setAreaCodeOmkTe(dtoObject.getAreaOMKTE().getCode());
        addresses.setAreaTeName(dtoObject.getAreaOMKTE().getName());
        addresses.setAreaTeTypeName(dtoObject.getAreaOMKTE().getType().getFull());
        addresses.setAreaTeTypeNameShort(dtoObject.getAreaOMKTE().getType().getShort());

        addresses.setAreaCode(dtoObject.getArea().getCode());
        addresses.setAreaBtiCode(dtoObject.getArea().getCodeBTI());
        addresses.setAreaName(dtoObject.getArea().getName());
        addresses.setAreaTypeName(dtoObject.getArea().getType().getFull());
        addresses.setAreaTypeNameShort(dtoObject.getArea().getType().getShort());

        addresses.setCityCode(dtoObject.getCity().getCode());
        addresses.setCityBtiCode(dtoObject.getCity().getCodeBTI());
        addresses.setCityName(dtoObject.getCity().getName());
        addresses.setCityTypeName(dtoObject.getCity().getType().getFull());
        addresses.setCityTypeNameShort(dtoObject.getCity().getType().getShort());

        addresses.setPlaceCode(dtoObject.getPlace().getCode());
        addresses.setPlaceBtiCode(dtoObject.getPlace().getCodeBTI());
        addresses.setPlaceName(dtoObject.getPlace().getName());
        addresses.setPlaceTypeName(dtoObject.getPlace().getType().getFull());
        addresses.setPlaceTypeNameShort(dtoObject.getPlace().getType().getShort());

        addresses.setPlanCode(dtoObject.getPlan().getCode());
        addresses.setPlanBtiCode(dtoObject.getPlan().getCodeBTI());
        addresses.setPlanName(dtoObject.getPlan().getName());
        addresses.setPlanTypeName(dtoObject.getPlan().getType().getFull());
        addresses.setPlanTypeNameShort(dtoObject.getPlan().getType().getShort());

        addresses.setStreetCode(dtoObject.getStreet().getCode());
        addresses.setStreetBtiCode(dtoObject.getStreet().getCodeBTI());
        addresses.setStreetName(dtoObject.getStreet().getName());
        addresses.setStreetTypeName(dtoObject.getStreet().getType().getFull());
        addresses.setStreetTypeNameShort(dtoObject.getStreet().getType().getShort());

        if (dtoObject.getBuilding() != null) {
            if (dtoObject.getBuilding().getHouse() != null) {
                addresses.setL1Type(dtoObject.getBuilding().getHouse().getType().getFull());
                addresses.setL1TypeShort(dtoObject.getBuilding().getHouse().getType().getShort());
                addresses.setL1Value(dtoObject.getBuilding().getHouse().getName());
            }

            if (dtoObject.getBuilding().getBuild() != null) {
                addresses.setL2Type(dtoObject.getBuilding().getBuild().getType().getFull());
                addresses.setL2TypeShort(dtoObject.getBuilding().getBuild().getType().getShort());
                addresses.setL2Value(dtoObject.getBuilding().getBuild().getName());
            }

            if (dtoObject.getBuilding().getConstruction() != null) {
                addresses.setL3Type(dtoObject.getBuilding().getConstruction().getType().getFull());
                addresses.setL3TypeShort(dtoObject.getBuilding().getConstruction().getType().getShort());
                addresses.setL3Value(dtoObject.getBuilding().getConstruction().getName());
            }
        }

        return addresses;
    }
}