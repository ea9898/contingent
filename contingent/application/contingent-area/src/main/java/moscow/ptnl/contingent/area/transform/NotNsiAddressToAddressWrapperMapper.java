package moscow.ptnl.contingent.area.transform;

import moscow.ptnl.contingent.area.model.area.AddressWrapper;
import moscow.ptnl.contingent.area.repository.nsi.AddressFormingElementRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import ru.mos.emias.contingent2.core.NotNsiAddress;

@Component
public class NotNsiAddressToAddressWrapperMapper implements Transform<NotNsiAddress, AddressWrapper> {

    @Autowired
    private AddressFormingElementRepository addressFormingElementRepository;

    @Override
    public NotNsiAddress entityToDtoTransform(AddressWrapper entityObject) {
        return null;
    }

    @Override
    public AddressWrapper dtoToEntityTransform(NotNsiAddress dtoObject) {
        AddressWrapper wrapper = new AddressWrapper();
        wrapper.notNsiAddress = dtoObject;
        wrapper.addressFormingElement = addressFormingElementRepository.getAddressFormingElements(
                dtoObject.getParentId(), dtoObject.getLevelParentId()).get(0);
        return wrapper;
    }
}
