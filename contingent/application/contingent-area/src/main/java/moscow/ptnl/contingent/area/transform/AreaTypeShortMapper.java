package moscow.ptnl.contingent.area.transform;

import moscow.ptnl.contingent.nsi.domain.area.AreaType;
import org.springframework.stereotype.Component;
import ru.mos.emias.contingent2.core.AreaTypeShort;

@Component
public class AreaTypeShortMapper implements Transform<AreaTypeShort, AreaType> {

    @Override
    public AreaTypeShort entityToDtoTransform(AreaType entityObject) {
        AreaTypeShort allocationOrder = new AreaTypeShort();
        allocationOrder.setCode(entityObject.getCode());
        allocationOrder.setName(entityObject.getTitle());

        return allocationOrder;
    }

    @Override
    public AreaType dtoToEntityTransform(AreaTypeShort dtoObject) {
        return null;
    }
}
