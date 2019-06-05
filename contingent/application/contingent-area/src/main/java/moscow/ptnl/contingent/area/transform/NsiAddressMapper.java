package moscow.ptnl.contingent.area.transform;

import org.springframework.stereotype.Component;
import ru.mos.emias.contingent2.core.NsiAddress;

@Component
public class NsiAddressMapper implements Transform<ru.mos.emias.contingent2.core.NsiAddress, moscow.ptnl.contingent.area.model.area.NsiAddress> {

    @Override
    public NsiAddress entityToDtoTransform(moscow.ptnl.contingent.area.model.area.NsiAddress object) {
        NsiAddress nsiAddress = new NsiAddress();
        nsiAddress.setGlobalId(object.getGlobalId());
        nsiAddress.setLevelAddress(object.getLevelAddress());
        return nsiAddress;
    }

    @Override
    public moscow.ptnl.contingent.area.model.area.NsiAddress dtoToEntityTransform(NsiAddress dtoObject) {
        moscow.ptnl.contingent.area.model.area.NsiAddress nsiAddress = new moscow.ptnl.contingent.area.model.area.NsiAddress();
        nsiAddress.setGlobalId(dtoObject.getGlobalId());
        nsiAddress.setLevelAddress(dtoObject.getLevelAddress());
        return nsiAddress;
    }

}
