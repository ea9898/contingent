package moscow.ptnl.contingent.area.transform.model.esu;

import moscow.ptnl.contingent.area.transform.Transform;
import moscow.ptnl.contingent.area.transform.model.XMLGregorianCalendarMapper;
import moscow.ptnl.contingent2.area.info.AreaInfoEvent;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class AreaInfoEventMapper implements Transform<AreaInfoEvent, moscow.ptnl.contingent.domain.esu.event.AreaInfoEvent> {

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
    public AreaInfoEvent entityToDtoTransform(moscow.ptnl.contingent.domain.esu.event.AreaInfoEvent entity) {
        AreaInfoEvent event = new AreaInfoEvent();
        moscow.ptnl.contingent.area.entity.area.Area area = entity.getArea();

        event.setOperationDate(gregorianCalendarMapper.entityToDtoTransform(entity.getOperationDate()));
        event.setOperationType(entity.getOperationType());
        event.setAreaId(area.getId());
        event.setMuId(area.getMuId());
        event.setAreaType(area.getAreaType().getCode());
        event.setArchive(area.getArchived());
        //Todo исправить обязательность поля в событии или участке
        event.setNumber(area.getNumber() == null ? 0 : area.getNumber());
        event.setName(area.getDescription());
        event.setAutoAssignForAttachment(Boolean.TRUE.equals(area.getAutoAssignForAttach()));
        event.setResidentsBindRate((area.getAreaType().getResidentsBindRate() != null) ? area.getAreaType().getResidentsBindRate().longValue() : null);
        event.setAreaRestriction(areaRestrictionMapper.entityToDtoTransform(area));
        if (area.getActualMainMedicalEmployees() != null && !area.getActualMainMedicalEmployees().isEmpty()) {
            event.setMainEmployees(mainEmployeesMapper.entityToDtoTransform(area.getActualMainMedicalEmployees()));
        }
        if (area.getActualReplacementMedicalEmployees() != null && !area.getActualReplacementMedicalEmployees().isEmpty()) {
            event.setReplacementEmployees(replacementEmployeesMapper.entityToDtoTransform(area.getActualReplacementMedicalEmployees()));
        }
        event.setAddresses(addressesMapper.entityToDtoTransform(area.getActualAreaAddresses()));

        return event;
    }

    @Override
    public moscow.ptnl.contingent.domain.esu.event.AreaInfoEvent dtoToEntityTransform(AreaInfoEvent dtoObject) {
        return null;
    }
}
