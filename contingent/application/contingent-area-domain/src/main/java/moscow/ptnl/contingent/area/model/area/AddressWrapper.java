package moscow.ptnl.contingent.area.model.area;

import moscow.ptnl.contingent.area.entity.area.Addresses;
import moscow.ptnl.contingent.area.entity.area.MoAddress;
import moscow.ptnl.contingent.nsi.domain.area.NsiAddressFormingElement;
import moscow.ptnl.contingent.nsi.domain.area.NsiBuildingRegistry;

public class AddressWrapper {

    public NsiAddress nsiAddress;

    public NsiBuildingRegistry buildingRegistry;

    public NsiAddressFormingElement addressFormingElement;

    public Addresses address;

    public MoAddress moAddress;

    private AddressLevelType addressLevelType;

    private Long brGlobalId;

    public AddressWrapper() {
    }

    public AddressWrapper(NsiAddress nsiAddress) {
        this.nsiAddress = nsiAddress;
    }

    public AddressWrapper(AddressWrapper addressWrapper) {
        this.nsiAddress = addressWrapper.nsiAddress;
        this.buildingRegistry = addressWrapper.buildingRegistry;
        this.addressFormingElement = addressWrapper.addressFormingElement;
        this.address = addressWrapper.address;
        this.moAddress = addressWrapper.moAddress;
    }

    public AddressWrapper(NsiAddressFormingElement addressFormingElement) {
        this.addressFormingElement = addressFormingElement;
    }

    public NsiAddress getNsiAddress() {
        return nsiAddress;
    }

    public void setNsiAddress(NsiAddress nsiAddress) {
        this.nsiAddress = nsiAddress;
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

    public Addresses getAddress() {
        return address;
    }

    public void setAddress(Addresses address) {
        this.address = address;
    }

    public MoAddress getMoAddress() {
        return moAddress;
    }

    public void setMoAddress(MoAddress moAddress) {
        this.moAddress = moAddress;
    }

    public Long getBrGlobalId() { return brGlobalId; }

    public void setBrGlobalId(Long brGlobalId) { this.brGlobalId = brGlobalId; }

    public AddressLevelType getAddressLevelType() { return addressLevelType; }

    public void setAddressLevelType(AddressLevelType addressLevelType) { this.addressLevelType = addressLevelType; }
}
