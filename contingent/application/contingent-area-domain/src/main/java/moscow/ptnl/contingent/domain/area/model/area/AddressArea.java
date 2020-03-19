package moscow.ptnl.contingent.domain.area.model.area;

import moscow.ptnl.contingent.domain.area.entity.area.Addresses;

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
}
