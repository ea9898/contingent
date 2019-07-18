package moscow.ptnl.contingent.area.transform.model.esu;

import moscow.ptnl.contingent.domain.esu.event.AttachOnAreaChangeEvent;
import moscow.ptnl.contingent.area.transform.Transform;
import moscow.ptnl.contingent.area.transform.model.XMLGregorianCalendarMapper;
import moscow.ptnl.contingent2.attachment.changearea.event.AreaRestriction;
import moscow.ptnl.contingent2.attachment.changearea.event.AttachOnAreaChange;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class AttachOnAreaChangeMapper implements Transform<AttachOnAreaChange, moscow.ptnl.contingent.domain.esu.event.AttachOnAreaChangeEvent> {

    @Autowired
    private XMLGregorianCalendarMapper gregorianCalendarMapper;

    @Override
    public AttachOnAreaChange entityToDtoTransform(moscow.ptnl.contingent.domain.esu.event.AttachOnAreaChangeEvent entity) {
        AttachOnAreaChange event = new AttachOnAreaChange();
        moscow.ptnl.contingent.area.entity.area.Area area = entity.getArea();

        event.setOperationDate(gregorianCalendarMapper.entityToDtoTransform(entity.getOperationDate()));
        AttachOnAreaChange.DependentArea dependentArea = new AttachOnAreaChange.DependentArea();
        dependentArea.setAreaId(area.getId());
        AreaRestriction restriction = new AreaRestriction();
        boolean empty = area.getAgeMin() == null && area.getAgeMax() == null &&
                area.getAgeMMin() == null && area.getAgeWMax() == null &&
                area.getAgeWMin() == null && area.getAgeWMax() == null;

        restriction.setMinAge(empty ? area.getAreaType().getAgeMin() : area.getAgeMin());
        restriction.setMinAgeMale(empty ? area.getAreaType().getAgeMMin() : area.getAgeMMin());
        restriction.setMinAgeFemale(empty ? area.getAreaType().getAgeWMin() : area.getAgeWMin());
        restriction.setMaxAge(empty ? area.getAreaType().getAgeMax() : area.getAgeMax());
        restriction.setMaxAgeMale(empty ? area.getAreaType().getAgeMMax() : area.getAgeMMax());
        restriction.setMaxAgeFemale(empty ? area.getAreaType().getAgeWMax() : area.getAgeWMax());
        dependentArea.setAreaRestriction(restriction);
        dependentArea.setMoId(area.getMoId());
        dependentArea.setMuId(area.getMuId());
        dependentArea.getPolicyType().add(1L); //CONTINGENT2-209
        event.setDependentArea(dependentArea);

        if (AttachOnAreaChangeEvent.OperationType.CREATE.equals(entity.getOperationType())) {
            event.getPrimaryAreaAdd().addAll(entity.getPrimaryAreaIds());
        }
        else {
            event.getPrimaryAreaDel().addAll(entity.getPrimaryAreaIds());
        }
        return event;
    }

    @Override
    public moscow.ptnl.contingent.domain.esu.event.AttachOnAreaChangeEvent dtoToEntityTransform(AttachOnAreaChange dtoObject) {
        return null;
    }
}
