package moscow.ptnl.contingent.area.transform.model.esu;

import moscow.ptnl.contingent.area.transform.Transform;
import moscow.ptnl.contingent.area.transform.model.XMLGregorianCalendarMapper;
import moscow.ptnl.contingent2.area.info.AreaInfoEvent;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class AreaInfoEventMapper implements Transform<AreaInfoEvent, moscow.ptnl.contingent.area.model.esu.AreaInfoEvent> {

    @Autowired
    private XMLGregorianCalendarMapper gregorianCalendarMapper;

    @Autowired
    private AreaRestrictionMapper areaRestrictionMapper;

    @Autowired
    private MainEmployeesMapper mainEmployeesMapper;

    @Autowired
    private ReplacementEmployeesMapper replacementEmployeesMapper;

    @Autowired
    private AddressesMapper addressesMapper;

    @Override
    public AreaInfoEvent entityToDtoTransform(moscow.ptnl.contingent.area.model.esu.AreaInfoEvent entity) {
        AreaInfoEvent event = new AreaInfoEvent();
        moscow.ptnl.contingent.area.entity.area.Area area = entity.getArea();

        event.setOperationDate(gregorianCalendarMapper.entityToDtoTransform(entity.getOperationDate()));
        event.setOperationType(entity.getOperationType());
        event.setAreaId(area.getId());
        event.setMuId(area.getMuId());
        event.setAreaType(area.getAreaType().getCode());
        event.setArchive(area.getArchived());
        event.setNumber(area.getNumber());
        event.setName(area.getDescription());
        event.setAutoAssignForAttachment(area.getAutoAssignForAttach());
        event.setResidentsBindRate(area.getAreaType().getResidentsBindRate().longValue());
        event.setAreaRestriction(areaRestrictionMapper.entityToDtoTransform(area));
        event.setMainEmployees(mainEmployeesMapper.entityToDtoTransform(area.getActualMainMedicalEmployees()));
        event.setReplacementEmployees(replacementEmployeesMapper.entityToDtoTransform(area.getActualReplacementMedicalEmployees()));
        event.setAddresses(addressesMapper.entityToDtoTransform(area.getActualAreaAddresses()));

        return event;
    }

    @Override
    public moscow.ptnl.contingent.area.model.esu.AreaInfoEvent dtoToEntityTransform(AreaInfoEvent dtoObject) {
        return null;
    }
}
