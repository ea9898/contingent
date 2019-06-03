package moscow.ptnl.contingent.area.transform.model.esu;

import org.springframework.stereotype.Component;

@Component
public class AreaRestrictionMapper/* implements Transform<AreaRestriction, moscow.ptnl.contingent.area.entity.area.Area>*/ {

//    @Override
//    public AreaRestriction entityToDtoTransform(moscow.ptnl.contingent.area.entity.area.Area entity) {
//        AreaRestriction restriction = new AreaRestriction();
//        boolean empty = entity.getAgeMin() == null && entity.getAgeMax() == null &&
//                entity.getAgeMMin() == null && entity.getAgeWMax() == null &&
//                entity.getAgeWMin() == null && entity.getAgeWMax() == null;
//
//        restriction.setMinAge(empty ? entity.getAreaType().getAgeMin() : entity.getAgeMin());
//        restriction.setMinAgeMale(empty ? entity.getAreaType().getAgeMMin() : entity.getAgeMMin());
//        restriction.setMinAgeFemale(empty ? entity.getAreaType().getAgeWMin() : entity.getAgeWMin());
//        restriction.setMaxAge(empty ? entity.getAreaType().getAgeMax() : entity.getAgeMax());
//        restriction.setMaxAgeMale(empty ? entity.getAreaType().getAgeMMax() : entity.getAgeMMax());
//        restriction.setMaxAgeFemale(empty ? entity.getAreaType().getAgeWMax() : entity.getAgeWMax());
//
//        return restriction;
//    }
//
//    @Override
//    public moscow.ptnl.contingent.area.entity.area.Area dtoToEntityTransform(AreaRestriction dtoObject) {
//        return null;
//    }
}
