package moscow.ptnl.contingent.area.transform.model.esu;

import moscow.ptnl.contingent.area.transform.Transform;
import moscow.ptnl.contingent.area.transform.model.XMLGregorianCalendarMapper;
import moscow.ptnl.contingent2.area.info.AreaInfoEvent;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;


@Component
public class AreaEventMapper implements Transform<AreaInfoEvent, moscow.ptnl.contingent.domain.esu.event.AreaEvent> {

    @Autowired
    private AreaInfoEventMapper areaInfoEventMapper;

    @Autowired
    private AreaUpdateEventMapper areaUpdateEventMapper;

    @Autowired
    private XMLGregorianCalendarMapper gregorianCalendarMapper;

    @Override
    public AreaInfoEvent entityToDtoTransform(moscow.ptnl.contingent.domain.esu.event.AreaEvent entityObject) {
        AreaInfoEvent event = new AreaInfoEvent();
//        event.setInformDate(gregorianCalendarMapper.entityToDtoTransform(LocalDateTime.now()));
//        event.setOperationDate(gregorianCalendarMapper.entityToDtoTransform(entityObject.getArea().getUpdateDate()));
//
//        if (OperationType.CREATE.equals(entityObject.getOperationType())) {
//            event.setCreateАrea(areaInfoEventMapper.entityToDtoTransform(entityObject));
//        }
//        else if (OperationType.UPDATE.equals(entityObject.getOperationType())) {
//            event.setUpdateАrea(areaUpdateEventMapper.entityToDtoTransform(entityObject));
//        }
//        else if (OperationType.CLOSE.equals(entityObject.getOperationType())) {
//            AreaEvent.CloseАrea closeEvent = new AreaEvent.CloseАrea();
//            closeEvent.setAreaId(entityObject.getArea().getId());
//            event.setCloseАrea(closeEvent);
//        }
        return event;
    }

    @Override
    public moscow.ptnl.contingent.domain.esu.event.AreaEvent dtoToEntityTransform(AreaInfoEvent dtoObject) {
        return null;
    }
}