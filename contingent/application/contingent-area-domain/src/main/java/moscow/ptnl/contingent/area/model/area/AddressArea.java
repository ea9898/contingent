package moscow.ptnl.contingent.area.model.area;

import moscow.ptnl.contingent.area.entity.area.Addresses;
import moscow.ptnl.contingent.nsi.domain.area.NsiAddressFormingElement;
import moscow.ptnl.contingent.nsi.domain.area.NsiBuildingRegistry;

/*
Выходной объекта метода getAreaAddress из слоя Service
 */
public class AddressArea {

    private Long areaAddressId;

    private Addresses addresses;

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
