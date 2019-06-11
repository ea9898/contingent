package moscow.ptnl.contingent.area.model.area;

import moscow.ptnl.contingent.area.entity.area.Addresses;
import moscow.ptnl.contingent.area.entity.area.MoAddress;
import moscow.ptnl.contingent.area.entity.nsi.AddressFormingElement;
import moscow.ptnl.contingent.area.entity.nsi.BuildingRegistry;

public class AddressWrapper {

    public NsiAddress nsiAddress;

    public NotNsiAddress notNsiAddress;

    public BuildingRegistry buildingRegistry;

    public AddressFormingElement addressFormingElement;

    public Addresses address;

    public MoAddress moAddress;

    private AddressLevelType addressLevelType;

    private Long brGlobalId;

    public AddressWrapper() {
    }

    public AddressWrapper(NsiAddress nsiAddress) {
        this.nsiAddress = nsiAddress;
    }

    public AddressWrapper(NotNsiAddress notNsiAddress) { this.notNsiAddress = notNsiAddress; }

    public AddressWrapper(AddressWrapper addressWrapper) {
        this.nsiAddress = addressWrapper.nsiAddress;
        this.notNsiAddress = addressWrapper.notNsiAddress;
        this.buildingRegistry = addressWrapper.buildingRegistry;
        this.addressFormingElement = addressWrapper.addressFormingElement;
        this.address = addressWrapper.address;
        this.moAddress = addressWrapper.moAddress;
    }

    public AddressWrapper(AddressFormingElement addressFormingElement) {
        this.addressFormingElement = addressFormingElement;
    }

    public AddressWrapper(BuildingRegistry buildingRegistry, AddressFormingElement addressFormingElement) {
        this.buildingRegistry = buildingRegistry;
        this.addressFormingElement = addressFormingElement;
    }

    public NsiAddress getNsiAddress() {
        return nsiAddress;
    }

    public void setNsiAddress(NsiAddress nsiAddress) {
        this.nsiAddress = nsiAddress;
    }

    public NotNsiAddress getNotNsiAddress() {
        return notNsiAddress;
    }

    public void setNotNsiAddress(NotNsiAddress notNsiAddress) {
        this.notNsiAddress = notNsiAddress;
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
