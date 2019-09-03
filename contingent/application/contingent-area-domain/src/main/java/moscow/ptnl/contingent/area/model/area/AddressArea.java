package moscow.ptnl.contingent.area.model.area;

import moscow.ptnl.contingent.nsi.domain.area.NsiAddressFormingElement;
import moscow.ptnl.contingent.nsi.domain.area.NsiBuildingRegistry;

/*
Выходной объекта метода getAreaAddress из слоя Service
 */
public class AddressArea {

    private Long areaAddressId;

    private NsiBuildingRegistry buildingRegistry;

    private NsiAddressFormingElement addressFormingElement;

    public AddressArea(Long areaAddressId, NsiAddressFormingElement addressFormingElement) {
        this.areaAddressId = areaAddressId;
        this.addressFormingElement = addressFormingElement;
    }

    public AddressArea(Long areaAddressId, NsiBuildingRegistry buildingRegistry, NsiAddressFormingElement addressFormingElement) {
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

    public NsiBuildingRegistry getBuildingRegistry() {
        return buildingRegistry;
    }

    public void setBuildingRegistry(NsiBuildingRegistry buildingRegistry) {
        this.buildingRegistry = buildingRegistry;
    }

    public NsiAddressFormingElement getAddressFormingElement() {
        return addressFormingElement;
    }

    public void setAddressFormingElement(NsiAddressFormingElement addressFormingElement) {
        this.addressFormingElement = addressFormingElement;
    }
}
