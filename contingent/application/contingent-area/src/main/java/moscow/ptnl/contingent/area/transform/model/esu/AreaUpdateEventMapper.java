package moscow.ptnl.contingent.area.transform.model.esu;

import moscow.ptnl.contingent.area.event.AreaUpdateEvent;
import moscow.ptnl.contingent.area.model.esu.AreaEvent;
import moscow.ptnl.contingent.area.transform.Transform;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Objects;

@Component
public class AreaUpdateEventMapper implements Transform<AreaUpdateEvent, AreaEvent> {

    @Autowired
    private PrimaryAreaTypeCodesEventMapper primaryAreaTypeCodesEventMapper;

    @Override
    public AreaUpdateEvent entityToDtoTransform(moscow.ptnl.contingent.area.model.esu.AreaEvent entityObject) {
        AreaUpdateEvent event = new AreaUpdateEvent();
        moscow.ptnl.contingent.area.entity.area.Area area = entityObject.getArea();
        moscow.ptnl.contingent.area.entity.area.Area oldArea = entityObject.getOldArea();

        event.setAreaId(area.getId());
        event.setMoId(Objects.equals(oldArea.getMoId(), area.getMoId()) ? null : area.getMoId());
        event.setMuId(Objects.equals(oldArea.getMuId(), area.getMuId()) ? null : area.getMuId());
        event.setDescription(Objects.equals(oldArea.getDescription(), area.getDescription()) ? null : area.getDescription());
        event.setAreaTypeClassCode(Objects.equals(oldArea.getAreaType().getClassAreaType().getCode(),
                area.getAreaType().getClassAreaType().getCode()) ? null : area.getAreaType().getClassAreaType().getCode());
        event.setAreaTypeKindCode(Objects.equals(oldArea.getAreaType().getKindAreaType().getCode(),
                area.getAreaType().getKindAreaType().getCode()) ? null : area.getAreaType().getKindAreaType().getCode());
        event.setAreaTypeCode(Objects.equals(oldArea.getAreaType().getCode(), area.getAreaType().getCode()) ? null : area.getAreaType().getCode());
        event.setAgeMin(Objects.equals(oldArea.getAgeMin(), area.getAgeMin()) ? null : area.getAgeMin());
        event.setAgeMax(Objects.equals(oldArea.getAgeMax(), area.getAgeMax()) ? null : area.getAgeMax());
        event.setAgeMinMale(Objects.equals(oldArea.getAgeMMin(), area.getAgeMMin()) ? null : area.getAgeMMin());
        event.setAgeMaxMale(Objects.equals(oldArea.getAgeMMax(), area.getAgeMMax()) ? null : area.getAgeMMax());
        event.setAgeMinFemale(Objects.equals(oldArea.getAgeWMin(), area.getAgeWMin()) ? null : area.getAgeWMin());
        event.setAgeMaxFemale(Objects.equals(oldArea.getAgeWMax(), area.getAgeWMax()) ? null : area.getAgeWMax());
        event.setIsAutoAssignForAttach(Objects.equals(oldArea.getAutoAssignForAttach(), area.getAutoAssignForAttach()) ? null : area.getAutoAssignForAttach());
        event.setAttachByMedicalReason(Objects.equals(oldArea.getAttachByMedicalReason(), area.getAttachByMedicalReason()) ? null : area.getAttachByMedicalReason());

        event.setAddPrimaryАreaTypes(primaryAreaTypeCodesEventMapper.entityToDtoTransform(entityObject.getAddPrimaryAreaTypes()));
        event.setDeletePrimaryАreaTypes(primaryAreaTypeCodesEventMapper.entityToDtoTransform(entityObject.getDelPrimaryAreaTypes()));

        return event;
    }

    @Override
    public moscow.ptnl.contingent.area.model.esu.AreaEvent dtoToEntityTransform(AreaUpdateEvent dtoObject) {
        return null;
    }
}
