package moscow.ptnl.contingent.area.transform.model.esu;

import moscow.ptnl.contingent.area.transform.Transform;
import moscow.ptnl.contingent.util.XMLGregorianCalendarMapper;
import moscow.ptnl.contingent2.area.info.AreaInfoEvent;
import moscow.ptnl.util.CollectionsUtil;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class AreaInfoEventMapper implements Transform<AreaInfoEvent, moscow.ptnl.contingent.domain.esu.event.AreaInfoEvent> {

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
        //1
        event.setOperationDate(XMLGregorianCalendarMapper.entityToDtoTransform(entity.getOperationDate()));
        event.setOperationType(entity.getOperationType());
        event.setAreaId(area.getId());
        event.setMuId(area.getMuId() == null ? area.getMoId() : area.getMuId());
        event.setAreaType(area.getAreaType().getCode());
        event.setArchive(area.getArchived());
        event.setNumber(area.getNumber());
        event.setName(area.getDescription());
        event.setAutoAssignForAttachment(Boolean.TRUE.equals(area.getAutoAssignForAttach()));
        event.setResidentsBindRate((area.getAreaType().getResidentsBindRate() != null) ? area.getAreaType().getResidentsBindRate().longValue() : null);
        event.setAreaRestriction(areaRestrictionMapper.entityToDtoTransform(area));
        //2
        if (!CollectionsUtil.isNullOrEmpty(area.getActualMainMedicalEmployees())) {
            event.setMainEmployees(mainEmployeesMapper.entityToDtoTransform(area.getActualMainMedicalEmployees()));
        }
        if (!CollectionsUtil.isNullOrEmpty(area.getActualReplacementMedicalEmployees())) {
            event.setReplacementEmployees(replacementEmployeesMapper.entityToDtoTransform(area.getActualReplacementMedicalEmployees()));
        }
        //3
        event.setAddresses(addressesMapper.entityToDtoTransform(area.getActualAreaAddresses()));

        return event;
    }

    @Override
    public moscow.ptnl.contingent.domain.esu.event.AreaInfoEvent dtoToEntityTransform(AreaInfoEvent dtoObject) {
        return null;
    }
}
