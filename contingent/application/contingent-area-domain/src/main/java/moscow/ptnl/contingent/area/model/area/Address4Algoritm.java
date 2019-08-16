package moscow.ptnl.contingent.area.model.area;

import moscow.ptnl.contingent.area.entity.area.Addresses;
import moscow.ptnl.contingent.area.entity.area.AreaAddress;

public class Address4Algoritm {

    private Long addressId;

    private int level;

    private Addresses addresses;

    public Address4Algoritm(Long addressId, int level) {
        this.addressId = addressId;
        this.level = level;
    }

    public Address4Algoritm(Addresses addresses) {
        //TODO fix
//        this(addresses.getBuildingRegistry() != null ? addresses.getBuildingRegistry().getId() :
//                addresses.getAddressFormingElement().getId(), addresses.getLevel());
    }

    public Address4Algoritm(AreaAddress areaAddress) {
        //TODO fix
//        this.level = areaAddress.getAddress().getLevel();
//        this.addresses = areaAddress.getAddress();
//        this.addressId = areaAddress.getAddress().getBuildingRegistry() != null ?
//            areaAddress.getAddress().getBuildingRegistry().getId() :
//            areaAddress.getAddress().getAddressFormingElement().getId();
    }


    public Long getAddressId() { return addressId; }

    public void setAddressId(Long addressId) { this.addressId = addressId; }

    public int getLevel() { return level; }

    public void setLevel(int level) { this.level = level; }

    public Addresses getAddresses() {
        return addresses;
    }

    public void setAddresses(Addresses addresses) {
        this.addresses = addresses;
    }
}
