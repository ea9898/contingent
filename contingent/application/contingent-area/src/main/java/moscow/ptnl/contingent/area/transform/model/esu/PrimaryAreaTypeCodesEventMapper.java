package moscow.ptnl.contingent.area.transform.model.esu;

import moscow.ptnl.contingent.area.entity.area.AreaToAreaType;
import moscow.ptnl.contingent2.area.event.PrimaryAreaTypeCodesEvent;
import moscow.ptnl.contingent.area.transform.Transform;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
public class PrimaryAreaTypeCodesEventMapper implements Transform<PrimaryAreaTypeCodesEvent, List<AreaToAreaType>> {

    @Override
    public PrimaryAreaTypeCodesEvent entityToDtoTransform(List<moscow.ptnl.contingent.area.entity.area.AreaToAreaType> entityObject) {
        if (entityObject == null || entityObject.isEmpty()) {
            return null;
        }
        PrimaryAreaTypeCodesEvent codes = new PrimaryAreaTypeCodesEvent();
        entityObject.stream()
                .filter(e -> e.getMuProfile().getAreaType() != null)
                .forEach(e -> codes.getPrimaryAreaTypeCode().add(e.getMuProfile().getAreaType().getCode()));

        return codes;
    }

    @Override
    public List<moscow.ptnl.contingent.area.entity.area.AreaToAreaType> dtoToEntityTransform(PrimaryAreaTypeCodesEvent dtoObject) {
        return null;
    }
}
