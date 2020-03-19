package moscow.ptnl.contingent.esuInputTasks;

import moscow.ptnl.contingent.domain.area.entity.area.Area;
import moscow.ptnl.contingent.nsi.domain.area.AreaType;
import moscow.ptnl.contingent.nsi.domain.area.PolicyTypeEnum;
import moscow.ptnl.contingent2.attachment.changeprimarea.event.AttachPrimaryPatientEvent;
import moscow.ptnl.contingent2.attachment.deparea.event.AreaRestriction;
import moscow.ptnl.contingent2.attachment.deparea.event.AttachToDependentAreaEvent;
import moscow.ptnl.util.XMLUtil;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.stream.Collectors;

@Component
public class AttachToDependentAreaEventMapper {


    public static AttachToDependentAreaEvent entityToDtoTransform(AttachPrimaryPatientEvent attachPrimaryPatientEvent,
                                                                  List<Area> dependentAreas) {
        AttachToDependentAreaEvent event = new AttachToDependentAreaEvent();
        event.setOperationDate(XMLUtil.getCurrentDate());
        event.setPatientEmiasId(attachPrimaryPatientEvent.getPatientEmiasId());
        event.setPrimaryAreaId(attachPrimaryPatientEvent.getPrimaryAreaId());
        event.getDependendAttachment().addAll(dependentAreas.stream().map(
                depArea -> {
                    AttachToDependentAreaEvent.DependendAttachment attachment = new AttachToDependentAreaEvent.DependendAttachment();
                    attachment.setMoId(depArea.getMoId());
                    attachment.setMuId(depArea.getMuId());
                    attachment.setAreaId(depArea.getId());
                    AreaRestriction restriction = new AreaRestriction();
                    AreaType depAreaType = depArea.getAreaType();
                    if (depAreaType.getGender() != null && depAreaType.getGender().length() != 0) {
                        restriction.setGender(Long.valueOf(depAreaType.getGender()));
                    }
                    if (depArea.getAgeMin() == null && depArea.getAgeMax() == null
                            && depArea.getAgeMMin() == null && depArea.getAgeMMax() == null
                            && depArea.getAgeWMin() == null && depArea.getAgeWMax() == null) {
                        restriction.setMinAge(depAreaType.getAgeMin());
                        restriction.setMaxAge(depAreaType.getAgeMax());
                        restriction.setMinAgeMale(depAreaType.getAgeMMin());
                        restriction.setMaxAgeMale(depAreaType.getAgeMMax());
                        restriction.setMinAgeFemale(depAreaType.getAgeWMin());
                        restriction.setMaxAgeFemale(depAreaType.getAgeWMax());
                    } else {
                        restriction.setMinAge(depArea.getAgeMin());
                        restriction.setMaxAge(depArea.getAgeMax());
                        restriction.setMinAgeMale(depArea.getAgeMMin());
                        restriction.setMaxAgeMale(depArea.getAgeMMax());
                        restriction.setMinAgeFemale(depArea.getAgeWMin());
                        restriction.setMaxAgeFemale(depArea.getAgeWMax());
                    }
                    attachment.setAreaRestriction(restriction);
                    attachment.setPolicyType(PolicyTypeEnum.OMS.getCode());
                    return attachment;
                }).collect(Collectors.toList()));
        return event;
    }

}
