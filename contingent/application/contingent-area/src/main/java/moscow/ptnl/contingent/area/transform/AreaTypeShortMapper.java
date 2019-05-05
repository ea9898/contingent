package moscow.ptnl.contingent.area.transform;

import org.springframework.stereotype.Component;
import ru.mos.emias.contingent2.core.AreaTypeShort;

@Component
public class AreaTypeShortMapper implements Transform<AreaTypeShort, moscow.ptnl.contingent.area.entity.nsi.AreaTypes> {

    @Override
    public AreaTypeShort entityToDtoTransform(moscow.ptnl.contingent.area.entity.nsi.AreaTypes entityObject) {
        AreaTypeShort allocationOrder = new AreaTypeShort();
        allocationOrder.setCode(entityObject.getCode());
        allocationOrder.setName(entityObject.getName());

        return allocationOrder;
    }

    @Override
    public moscow.ptnl.contingent.area.entity.nsi.AreaTypes dtoToEntityTransform(AreaTypeShort dtoObject) {
        return null;
    }
}
