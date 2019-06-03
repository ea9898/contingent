package moscow.ptnl.contingent.area.transform;

import moscow.ptnl.contingent.area.model.area.AddressLevelType;
import moscow.ptnl.contingent.area.model.area.AddressWrapper;
import moscow.ptnl.contingent.area.repository.nsi.AddressFormingElementRepository;
import moscow.ptnl.contingent.area.repository.nsi.BuildingRegistryRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import ru.mos.emias.contingent2.core.NsiAddress;

@Component
public class NsiAddressToAddressWrapperMapper implements Transform<NsiAddress, AddressWrapper> {

    @Autowired
    private AddressFormingElementRepository addressFormingElementRepository;

    @Autowired
    private BuildingRegistryRepository buildingRegistryRepository;


    @Override
    public NsiAddress entityToDtoTransform(AddressWrapper entityObject) {
        return null;
    }

    @Override
    public AddressWrapper dtoToEntityTransform(NsiAddress dtoObject) {
        AddressWrapper addressWrapper = new AddressWrapper(dtoObject);

        if (addressWrapper.getNsiAddress().getLevelAddress() != AddressLevelType.ID.getLevel()) {
            addressWrapper.setAddressFormingElement(addressFormingElementRepository.getAddressFormingElements(
                    addressWrapper.getNsiAddress().getGlobalId(), addressWrapper.getNsiAddress().getLevelAddress()).get(0));
        } else {
            addressWrapper.setBuildingRegistry(buildingRegistryRepository.getBuildingsRegistry(
                    addressWrapper.getNsiAddress().getGlobalId()).get(0));
        }

        return addressWrapper;
    }
}
