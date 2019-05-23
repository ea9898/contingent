package moscow.ptnl.contingent.area.model.area;

import moscow.ptnl.contingent.area.entity.area.Addresses;
import moscow.ptnl.contingent.area.entity.area.MoAddress;
import moscow.ptnl.contingent.area.entity.nsi.AddressFormingElement;
import moscow.ptnl.contingent.area.entity.nsi.RegistryBuilding;
import ru.mos.emias.contingent2.core.NotNsiAddress;
import ru.mos.emias.contingent2.core.NsiAddress;

public class AddressWrapper {

    public NsiAddress nsiAddress;

    public NotNsiAddress notNsiAddress;

    public RegistryBuilding registryBuilding;

    public AddressFormingElement addressFormingElement;

    public Addresses address;

    public MoAddress moAddress;

    public AddressWrapper() {
    }

    public AddressWrapper(NsiAddress nsiAddress) {
        this.nsiAddress = nsiAddress;
    }

    public AddressWrapper(AddressWrapper addressWrapper) {
        this.nsiAddress = addressWrapper.nsiAddress;
        this.notNsiAddress = addressWrapper.notNsiAddress;
        this.registryBuilding = addressWrapper.registryBuilding;
        this.addressFormingElement = addressWrapper.addressFormingElement;
        this.address = addressWrapper.address;
        this.moAddress = addressWrapper.moAddress;
    }
}
