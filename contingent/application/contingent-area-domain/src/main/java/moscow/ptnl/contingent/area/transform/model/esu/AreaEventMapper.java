package moscow.ptnl.contingent.area.transform.model.esu;

import moscow.ptnl.contingent2.area.event.AreaEvent;
import moscow.ptnl.contingent.area.model.esu.OperationType;
import moscow.ptnl.contingent.area.transform.Transform;
import moscow.ptnl.contingent.area.transform.model.XMLGregorianCalendarMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;

@Component
public class AreaEventMapper implements Transform<AreaEvent, moscow.ptnl.contingent.area.model.esu.AreaEvent> {

    @Autowired
    private AreaCreateEventMapper areaCreateEventMapper;

    @Autowired
    private AreaUpdateEventMapper areaUpdateEventMapper;

    @Autowired
    private XMLGregorianCalendarMapper gregorianCalendarMapper;

    @Override
    public AreaEvent entityToDtoTransform(moscow.ptnl.contingent.area.model.esu.AreaEvent entityObject) {
        AreaEvent event = new AreaEvent();
        event.setInformDate(gregorianCalendarMapper.entityToDtoTransform(LocalDateTime.now()));
        event.setOperationDate(gregorianCalendarMapper.entityToDtoTransform(entityObject.getArea().getUpdateDate()));

        if (OperationType.CREATE.equals(entityObject.getOperationType())) {
            event.setCreateАrea(areaCreateEventMapper.entityToDtoTransform(entityObject));
        }
        else if (OperationType.UPDATE.equals(entityObject.getOperationType())) {
            event.setUpdateАrea(areaUpdateEventMapper.entityToDtoTransform(entityObject));
        }
        else if (OperationType.CLOSE.equals(entityObject.getOperationType())) {
            AreaEvent.CloseАrea closeEvent = new AreaEvent.CloseАrea();
            closeEvent.setAreaId(entityObject.getArea().getId());
            event.setCloseАrea(closeEvent);
        }
        return event;
    }

    @Override
    public moscow.ptnl.contingent.area.model.esu.AreaEvent dtoToEntityTransform(AreaEvent dtoObject) {
        return null;
    }
}
