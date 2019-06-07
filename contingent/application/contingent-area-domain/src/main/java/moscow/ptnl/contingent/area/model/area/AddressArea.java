package moscow.ptnl.contingent.area.model.area;

import moscow.ptnl.contingent.area.entity.area.Addresses;
import moscow.ptnl.contingent.area.entity.nsi.AddressFormingElement;
import moscow.ptnl.contingent.area.entity.nsi.BuildingRegistry;

/*
Выходной объекта метода getAreaAddress из слоя Service
 */
public class AddressArea {

    private Long areaAddressId;

    private BuildingRegistry buildingRegistry;

    private AddressFormingElement addressFormingElement;

    public AddressArea(Long areaAddressId, AddressFormingElement addressFormingElement) {
        this.areaAddressId = areaAddressId;
        this.addressFormingElement = addressFormingElement;
    }

    public AddressArea(Long areaAddressId, BuildingRegistry buildingRegistry, AddressFormingElement addressFormingElement) {
        this.areaAddressId = areaAddressId;
        this.buildingRegistry = buildingRegistry;
        this.addressFormingElement = addressFormingElement;
    }

    public Long getAreaAddressId() {
        return areaAddressId;
    }

    public void setAreaAddressId(Long areaAddressId) {
        this.areaAddressId = areaAddressId;
    }

    public BuildingRegistry getBuildingRegistry() {
        return buildingRegistry;
    }

    public void setBuildingRegistry(BuildingRegistry buildingRegistry) {
        this.buildingRegistry = buildingRegistry;
    }

    public AddressFormingElement getAddressFormingElement() {
        return addressFormingElement;
    }

    public void setAddressFormingElement(AddressFormingElement addressFormingElement) {
        this.addressFormingElement = addressFormingElement;
    }
}
