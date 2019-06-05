package moscow.ptnl.contingent.area.transform;

import org.springframework.stereotype.Component;
import ru.mos.emias.contingent2.core.NotNsiAddress;
import ru.mos.emias.contingent2.core.NsiAddress;

@Component
public class NotNsiAddressMapper implements Transform<NotNsiAddress, moscow.ptnl.contingent.area.model.area.NotNsiAddress> {

    @Override
    public NotNsiAddress entityToDtoTransform(moscow.ptnl.contingent.area.model.area.NotNsiAddress object) {
        NotNsiAddress notNsiAddress = new NotNsiAddress();
        notNsiAddress.setLevelParentId(object.getLevelParentId());
        notNsiAddress.setParentId(object.getParentId());
        notNsiAddress.setHouseType(object.getHouseType());
        notNsiAddress.setHouse(object.getHouse());
        notNsiAddress.setBuildingType(object.getBuildingType());
        notNsiAddress.setBuilding(object.getBuilding());
        notNsiAddress.setConstructionType(object.getConstructionType());
        notNsiAddress.setConstruction(object.getConstruction());
        return notNsiAddress;
    }

    @Override
    public moscow.ptnl.contingent.area.model.area.NotNsiAddress dtoToEntityTransform(NotNsiAddress object) {
        moscow.ptnl.contingent.area.model.area.NotNsiAddress notNsiAddress = new moscow.ptnl.contingent.area.model.area.NotNsiAddress();
        notNsiAddress.setLevelParentId(object.getLevelParentId());
        notNsiAddress.setParentId(object.getParentId());
        notNsiAddress.setHouseType(object.getHouseType());
        notNsiAddress.setHouse(object.getHouse());
        notNsiAddress.setBuildingType(object.getBuildingType());
        notNsiAddress.setBuilding(object.getBuilding());
        notNsiAddress.setConstructionType(object.getConstructionType());
        notNsiAddress.setConstruction(object.getConstruction());
        return notNsiAddress;
    }

}
