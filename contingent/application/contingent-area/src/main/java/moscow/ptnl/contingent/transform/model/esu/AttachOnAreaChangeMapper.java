package moscow.ptnl.contingent.transform.model.esu;

import moscow.ptnl.contingent.domain.area.entity.Area;
import moscow.ptnl.contingent.domain.esu.event.AttachOnAreaChangeEvent;
import moscow.ptnl.contingent.transform.Transform;
import moscow.ptnl.contingent.util.XMLGregorianCalendarMapper;
import moscow.ptnl.contingent2.attachment.changearea.event.AreaRestriction;
import moscow.ptnl.contingent2.attachment.changearea.event.AttachOnAreaChange;
import org.springframework.stereotype.Component;

@Component
public class AttachOnAreaChangeMapper implements Transform<AttachOnAreaChange, moscow.ptnl.contingent.domain.esu.event.AttachOnAreaChangeEvent> {

    @Override
    public AttachOnAreaChange entityToDtoTransform(moscow.ptnl.contingent.domain.esu.event.AttachOnAreaChangeEvent entity) {
        AttachOnAreaChange event = new AttachOnAreaChange();
        Area area = entity.getArea();

        event.setOperationDate(XMLGregorianCalendarMapper.entityToDtoTransform(entity.getOperationDate()));
        AttachOnAreaChange.DependendArea dependendArea = new AttachOnAreaChange.DependendArea();
        dependendArea.setAreaId(area.getId());
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
        dependendArea.setAreaRestriction(restriction);
        dependendArea.setMoId(area.getMoId());
        dependendArea.setMuId(area.getMuId());
        dependendArea.getPolicyType().add(1L); //CONTINGENT2-209
        event.setDependendArea(dependendArea);

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
