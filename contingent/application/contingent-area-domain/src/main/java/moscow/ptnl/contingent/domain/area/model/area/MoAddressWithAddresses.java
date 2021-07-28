package moscow.ptnl.contingent.domain.area.model.area;

import moscow.ptnl.contingent.domain.area.entity.AddressAllocationOrders;
import moscow.ptnl.contingent.domain.area.entity.Addresses;
import moscow.ptnl.contingent.domain.area.entity.AreaAddress;
import moscow.ptnl.contingent.domain.area.entity.MoAddress;
import moscow.ptnl.contingent.nsi.domain.area.AreaType;

import java.util.List;

/**
 * @author sorlov
 */
public class MoAddressWithAddresses {

    public final MoAddress moAddress;
    public final AddressAllocationOrders addressAllocationOrder;
    public final AddressAllocationOrders addressRejectOrder;
    public final AreaType areaType;
    public final Addresses address;
    public final List<AreaAddress> areaAddresses;

    public MoAddressWithAddresses(MoAddress moAddress, List<AreaAddress> areaAddresses) {
        this.moAddress = moAddress;
        this.addressAllocationOrder = moAddress.getAddressAllocationOrder();
        this.addressRejectOrder = moAddress.getAddressRejectOrder();
        this.areaType = moAddress.getAreaType();
        this.address = moAddress.getAddress();
        this.areaAddresses = areaAddresses;
    }
}
