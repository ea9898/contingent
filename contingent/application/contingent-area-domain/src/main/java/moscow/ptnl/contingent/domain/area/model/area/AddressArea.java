package moscow.ptnl.contingent.domain.area.model.area;

import moscow.ptnl.contingent.domain.area.entity.Addresses;

/*
Выходной объекта метода getAreaAddress из слоя Service
 */
public class AddressArea {

    private Long moId;

    private Long muId;

    private Long areaId;

    private Long areaAddressId;

    private Addresses addresses;

    public Long getMoId() {
        return moId;
    }

    public void setMoId(Long moId) {
        this.moId = moId;
    }

    public Long getMuId() {
        return muId;
    }

    public void setMuId(Long muId) {
        this.muId = muId;
    }

    public Long getAreaId() {
        return areaId;
    }

    public void setAreaId(Long areaId) {
        this.areaId = areaId;
    }

    public Long getAreaAddressId() {
        return areaAddressId;
    }

    public void setAreaAddressId(Long areaAddressId) {
        this.areaAddressId = areaAddressId;
    }

    public Addresses getAddresses() {
        return addresses;
    }

    public void setAddresses(Addresses addresses) {
        this.addresses = addresses;
    }

    public static AddressArea.Builder builder() {
        return new AddressArea.Builder();
    }

    public static class Builder {
        private final AddressArea addressArea;

        private Builder(){
            this.addressArea = new AddressArea();
        }

        public Builder moId(Long moId) {
            addressArea.setMoId(moId);
            return this;
        }

        public Builder muId(Long muId) {
            addressArea.setMuId(muId);
            return this;
        }

        public Builder areaId(Long areaId) {
            addressArea.setAreaId(areaId);
            return this;
        }

        public Builder areaAddressId(Long areaAddressId) {
            addressArea.setAreaAddressId(areaAddressId);
            return this;
        }

        public Builder addresses(Addresses addresses) {
            addressArea.setAddresses(addresses);
            return this;
        }

        public AddressArea build() {
            return addressArea;
        }
    }
}
