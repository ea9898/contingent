package moscow.ptnl.contingent.esuInputTasks;

import moscow.ptnl.contingent.area.entity.area.Area;
import moscow.ptnl.contingent.area.entity.nsi.AreaType;
import moscow.ptnl.contingent.area.entity.nsi.PolicyTypeEnum;
import moscow.ptnl.contingent2.attachment.deparea.event.AreaRestriction;
import moscow.ptnl.contingent2.attachment.deparea.event.AttachToDependentAreaEvent;
import moscow.ptnl.util.XMLUtil;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.stream.Collectors;

@Component
public class AttachToDependentAreaEventMapper {


    public static AttachToDependentAreaEvent entityToDtoTransform(Area area, AreaType areaType, List<Area> dependentAreas) {
        AttachToDependentAreaEvent event = new AttachToDependentAreaEvent();
        event.setOperationDate(XMLUtil.getCurrentDate());
        event.setPatientEmiasId(event.getPatientEmiasId());
        event.setPrimaryAreaId(event.getPrimaryAreaId());
        event.getDependendAttachment().addAll(dependentAreas.stream().map(
                depArea -> {
                    AttachToDependentAreaEvent.DependendAttachment dependentArea = new AttachToDependentAreaEvent.DependendAttachment();
                    dependentArea.setMoId(depArea.getMoId());
                    dependentArea.setMuId(depArea.getMuId());
                    dependentArea.setAreaId(depArea.getId());
                    AreaRestriction restriction = new AreaRestriction();
                    restriction.setGender(area.getAreaType().getCode());
                    AreaType depAreaType = area.getAreaType();
                    if (depAreaType.getAgeMin() == null && depAreaType.getAgeMax() == null
                            && depAreaType.getAgeMMin() == null && depAreaType.getAgeMMax() == null
                            && depAreaType.getAgeWMin() == null && depAreaType.getAgeWMax() == null) {
                        restriction.setMinAge(areaType.getAgeMin());
                        restriction.setMaxAge(areaType.getAgeMax());
                        restriction.setMinAgeMale(areaType.getAgeMMin());
                        restriction.setMaxAgeMale(areaType.getAgeMMax());
                        restriction.setMinAgeFemale(areaType.getAgeWMin());
                        restriction.setMaxAgeFemale(areaType.getAgeWMax());
                    } else {
                        restriction.setMinAge(depAreaType.getAgeMin());
                        restriction.setMaxAge(depAreaType.getAgeMax());
                        restriction.setMinAgeMale(depAreaType.getAgeMMin());
                        restriction.setMaxAgeMale(depAreaType.getAgeMMax());
                        restriction.setMinAgeFemale(depAreaType.getAgeWMin());
                        restriction.setMaxAgeFemale(depAreaType.getAgeWMax());
                    }
                    dependentArea.setAreaRestriction(restriction);
                    dependentArea.setPolicyType(PolicyTypeEnum.OMS.getCode());
                    return dependentArea;
                }).collect(Collectors.toList()));
        return event;
    }

}
