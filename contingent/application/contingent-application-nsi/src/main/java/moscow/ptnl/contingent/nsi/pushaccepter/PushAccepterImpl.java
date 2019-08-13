package moscow.ptnl.contingent.nsi.pushaccepter;

import moscow.ptnl.contingent.area.entity.nsi.AreaType;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeClass;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeKind;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeMedicalPositions;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeRelations;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeSpecializations;
import moscow.ptnl.contingent.nsi.pushaccepter.xmlparsing.Package;
import moscow.ptnl.contingent.nsi.pushaccepter.xmlparsingS.Table;
import moscow.ptnl.contingent.repository.nsi.AreaTypeMedicalPositionsCRUDRepository;
import moscow.ptnl.contingent.repository.nsi.AreaTypeRelationsCRUDRepository;
import moscow.ptnl.contingent.repository.nsi.AreaTypeSpecializationsCRUDRepository;
import moscow.ptnl.contingent.repository.nsi.AreaTypesCRUDRepository;
import moscow.ptnl.contingent.repository.nsi.ClassAreaTypesCRUDRepository;
import moscow.ptnl.contingent.repository.nsi.KindAreaTypesCRUDRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import static moscow.ptnl.contingent.nsi.pushaccepter.PushAccepterMapper.mapAreaType;
import static moscow.ptnl.contingent.nsi.pushaccepter.PushAccepterMapper.mapAreaTypeClass;
import static moscow.ptnl.contingent.nsi.pushaccepter.PushAccepterMapper.mapAreaTypeKind;
import static moscow.ptnl.contingent.nsi.pushaccepter.PushAccepterMapper.mapAreaTypeMedicalPositions;
import static moscow.ptnl.contingent.nsi.pushaccepter.PushAccepterMapper.mapAreaTypeRelations;
import static moscow.ptnl.contingent.nsi.pushaccepter.PushAccepterMapper.mapAreaTypeSpecializations;
import static moscow.ptnl.contingent.nsi.pushaccepter.PushAccepterMapper.saveOrDelete;

@Component
public class PushAccepterImpl extends PushAccepter {

    @Autowired
    AreaTypesCRUDRepository areaTypesCRUDRepository;

    @Autowired
    ClassAreaTypesCRUDRepository classAreaTypesCRUDRepository;

    @Autowired
    KindAreaTypesCRUDRepository kindAreaTypesCRUDRepository;

    @Autowired
    AreaTypeMedicalPositionsCRUDRepository areaTypeMedicalPositionsCRUDRepository;

    @Autowired
    AreaTypeRelationsCRUDRepository areaTypeRelationsCRUDRepository;

    @Autowired
    AreaTypeSpecializationsCRUDRepository areaTypeSpecializationsCRUDRepository;

    @Override
    public Answer getPushSpec(Table table) {
        return new Answer(true, "ok");
    }

    @Override
    public Answer getPushForm(String response) {
        return new Answer(true, "ok");
    }

    @Override
    public Answer getPush(Package pack) {
        switch (NsiTablesEnum.getByName(pack.catalog.name)) {
            case AREA_TYPE:
                AreaType areaType = mapAreaType(pack);
                saveOrDelete(areaTypesCRUDRepository, areaType, pack.catalog.data.action);
                break;
            case AREA_TYPE_CLASS:
                AreaTypeClass areaTypeClass = mapAreaTypeClass(pack);
                saveOrDelete(classAreaTypesCRUDRepository, areaTypeClass, pack.catalog.data.action);
                break;
            case AREA_TYPE_KIND:
                AreaTypeKind areaTypeKind = mapAreaTypeKind(pack);
                saveOrDelete(kindAreaTypesCRUDRepository, areaTypeKind, pack.catalog.data.action);
                break;
            case AREA_TYPE_MEDICAL_POSITIONS:
                AreaTypeMedicalPositions areaTypeMedicalPositions = mapAreaTypeMedicalPositions(pack);
                saveOrDelete(areaTypeMedicalPositionsCRUDRepository, areaTypeMedicalPositions, pack.catalog.data.action);
                break;
            case AREA_TYPE_RELATIONS:
                AreaTypeRelations areaTypeRelations = mapAreaTypeRelations(pack);
                saveOrDelete(areaTypeRelationsCRUDRepository, areaTypeRelations, pack.catalog.data.action);
                break;
            case AREA_TYPE_SPECIALIZATIONS:
                AreaTypeSpecializations areaTypeSpecializations = mapAreaTypeSpecializations(pack);
                saveOrDelete(areaTypeSpecializationsCRUDRepository, areaTypeSpecializations, pack.catalog.data.action);
        }
        return new Answer(true, "ok");
    }
}
