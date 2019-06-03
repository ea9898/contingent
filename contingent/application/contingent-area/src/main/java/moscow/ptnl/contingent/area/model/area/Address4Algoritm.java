package moscow.ptnl.contingent.area.model.area;

import moscow.ptnl.contingent.area.entity.area.Addresses;

public class Address4Algoritm {

    private Long addressId;

    private int level;

    public Address4Algoritm(Long addressId, int level) {
        this.addressId = addressId;
        this.level = level;
    }

    public Address4Algoritm(Addresses addresses) {
        this(addresses.getBuildingRegistry() != null ? addresses.getBuildingRegistry().getId() :
                addresses.getAddressFormingElement().getId(), addresses.getLevel());
    }

    public Long getAddressId() { return addressId; }

    public void setAddressId(Long addressId) { this.addressId = addressId; }

    public int getLevel() { return level; }

    public void setLevel(int level) { this.level = level; }
}
