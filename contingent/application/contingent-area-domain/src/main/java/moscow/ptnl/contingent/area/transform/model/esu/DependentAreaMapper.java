package moscow.ptnl.contingent.area.transform.model.esu;

import moscow.ptnl.contingent.area.transform.Transform;
import moscow.ptnl.contingent2.attachment.changedeparea.event.AttachOnAreaRelationChangeEvent.DependentArea;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class DependentAreaMapper implements Transform<DependentArea, moscow.ptnl.contingent.area.entity.area.Area> {

    private static final Long DEFAULT_POLICY_TYPE = 1L;

    @Autowired
    private AreaRestrictionMapper areaRestrictionMapper;

    @Override
    public DependentArea entityToDtoTransform(moscow.ptnl.contingent.area.entity.area.Area entityObject) {
        DependentArea area = new DependentArea();
        area.setAreaId(entityObject.getId());
        area.setMoId(entityObject.getMoId());
        area.setMuId(entityObject.getMuId());
        area.setPolicyType(DEFAULT_POLICY_TYPE);
        area.setAreaRestriction(areaRestrictionMapper.entityToDtoTransform(entityObject));

        return area;
    }

    @Override
    public moscow.ptnl.contingent.area.entity.area.Area dtoToEntityTransform(DependentArea dtoObject) {
        return null;
    }
}
