package moscow.ptnl.contingent.area.transform.model.esu;

import moscow.ptnl.contingent.area.entity.area.AreaToAreaType;
import moscow.ptnl.contingent.area.entity.nsi.AreaType;
import moscow.ptnl.contingent.area.transform.Transform;
import moscow.ptnl.contingent.area.transform.model.XMLGregorianCalendarMapper;
import moscow.ptnl.contingent2.attachment.changedeparea.event.AttachOnAreaRelationChangeEvent;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.stream.Collectors;

@Component
public class AttachOnAreaRelationChangeEventMapper implements Transform<AttachOnAreaRelationChangeEvent, moscow.ptnl.contingent.area.model.esu.AreaEvent> {

    @Autowired
    private XMLGregorianCalendarMapper gregorianCalendarMapper;

    @Autowired
    private DependentAreaMapper dependentAreaMapper;

    @Override
    public AttachOnAreaRelationChangeEvent entityToDtoTransform(moscow.ptnl.contingent.area.model.esu.AreaEvent entityObject) {
        AttachOnAreaRelationChangeEvent event = new AttachOnAreaRelationChangeEvent();
        event.setOperationDate(gregorianCalendarMapper.entityToDtoTransform(entityObject.getArea().getUpdateDate()));
        event.setDependentArea(dependentAreaMapper.entityToDtoTransform(entityObject.getArea()));
        event.getPrimaryAreaAdd().addAll(entityObject.getAddPrimaryAreaTypes().stream()
                .map(AreaToAreaType::getAreaType)
                .map(AreaType::getCode)
                .collect(Collectors.toList()));
        event.getPrimaryAreaDel().addAll(entityObject.getDelPrimaryAreaTypes().stream()
                .map(AreaToAreaType::getAreaType)
                .map(AreaType::getCode)
                .collect(Collectors.toList()));

        return event;
    }

    @Override
    public moscow.ptnl.contingent.area.model.esu.AreaEvent dtoToEntityTransform(AttachOnAreaRelationChangeEvent dtoObject) {
        return null;
    }
}
