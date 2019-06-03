package moscow.ptnl.contingent.area.transform;

import moscow.ptnl.contingent.area.entity.nsi.AreaType;
import org.springframework.stereotype.Component;
import ru.mos.emias.contingent2.core.AreaTypeShort;

@Component
public class AreaTypeShortMapper implements Transform<AreaTypeShort, AreaType> {

    @Override
    public AreaTypeShort entityToDtoTransform(AreaType entityObject) {
        AreaTypeShort allocationOrder = new AreaTypeShort();
        allocationOrder.setCode(entityObject.getCode());
        allocationOrder.setName(entityObject.getName());

        return allocationOrder;
    }

    @Override
    public AreaType dtoToEntityTransform(AreaTypeShort dtoObject) {
        return null;
    }
}
