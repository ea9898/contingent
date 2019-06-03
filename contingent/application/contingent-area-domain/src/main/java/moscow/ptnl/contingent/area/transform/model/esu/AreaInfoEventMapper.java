package moscow.ptnl.contingent.area.transform.model.esu;

import moscow.ptnl.contingent.area.model.esu.AreaCreateEvent;
import moscow.ptnl.contingent.area.model.esu.AreaEvent;
import moscow.ptnl.contingent.area.transform.Transform;
import moscow.ptnl.contingent2.area.info.AreaInfoEvent;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class AreaInfoEventMapper implements Transform<AreaInfoEvent, AreaEvent> {

    @Autowired
    private PrimaryAreaTypeCodesEventMapper primaryAreaTypeCodesEventMapper;

    @Override
    public AreaInfoEvent entityToDtoTransform(moscow.ptnl.contingent.area.model.esu.AreaEvent entityObject) {
        AreaInfoEvent event = new AreaInfoEvent();
        moscow.ptnl.contingent.area.entity.area.Area area = entityObject.getArea();

//        event.setAreaId(area.getId());
//        event.setMoId(area.getMoId());
//        event.setMuId(area.getMuId());
//        event.setAreaNumber(area.getNumber());
//        event.setDescription(area.getDescription());
//        event.setAreaTypeClassCode(area.getAreaType().getClassAreaType().getCode());
//        event.setAreaTypeKindCode(area.getAreaType().getKindAreaType().getCode());
//        event.setAreaTypeCode(area.getAreaType().getCode());
//        event.setAgeMin(area.getAgeMin());
//        event.setAgeMax(area.getAgeMax());
//        event.setAgeMinMale(area.getAgeMMin());
//        event.setAgeMaxMale(area.getAgeMMax());
//        event.setAgeMinFemale(area.getAgeWMin());
//        event.setAgeMaxFemale(area.getAgeWMax());
//        event.setIsAutoAssignForAttach(area.getAutoAssignForAttach());
//        event.setAttachByMedicalReason(area.getAttachByMedicalReason());
//        event.setPrimaryAreaTypes(primaryAreaTypeCodesEventMapper.entityToDtoTransform(entityObject.getAddPrimaryAreaTypes()));

        return event;
    }

    @Override
    public moscow.ptnl.contingent.area.model.esu.AreaEvent dtoToEntityTransform(AreaInfoEvent dtoObject) {
        return null;
    }
}
