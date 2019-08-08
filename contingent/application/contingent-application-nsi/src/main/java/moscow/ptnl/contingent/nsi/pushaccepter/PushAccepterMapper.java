package moscow.ptnl.contingent.nsi.pushaccepter;

import moscow.ptnl.contingent.area.entity.nsi.AreaType;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeClass;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeKind;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeMedicalPositions;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeRelations;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeSpecializations;
import moscow.ptnl.contingent.nsi.pushaccepter.xmlparsing.Package;
import moscow.ptnl.contingent.repository.CommonRepository;

import java.io.Serializable;

public class PushAccepterMapper {

    public static <T, K extends Serializable> void saveOrDelete(CommonRepository<T, K> repository, T entity, String action) {
        if (action.equalsIgnoreCase(NsiActionsEnum.DELETED.toString())) {
            repository.delete(entity);
        } else {
            repository.save(entity);
        }
    }

    private static String getValue(Package pack, String parameterName) {
        return pack.catalog.data.attribute.stream().filter(atr -> atr.name.equalsIgnoreCase(parameterName)).findFirst()
                .orElseThrow(() -> new IllegalStateException(String.format("Нет такого поля %s в таблице %s", parameterName, pack.catalog.name)))
                .values.value.value;
    }

    private static boolean stringIsTrue(String booleanValue) {
        return booleanValue.equals("1");
    }

    public static AreaType mapAreaType(Package pack) {
        AreaType areaType = new AreaType();

        String code = getValue(pack, AreaType.FieldsEnum.CODE.toString());
        if (code.length() > 0) {
            areaType.setCode(Long.valueOf(code));
        }

        String title = getValue(pack, AreaType.FieldsEnum.TITLE.toString());
        if (title.length() > 0) {
            areaType.setTitle(title);
        }

        String areaTypeKindCode = getValue(pack, AreaType.FieldsEnum.AREA_TYPE_KIND_CODE.toString());
        if (areaTypeKindCode.length() > 0) {
            areaType.setAreaTypeKind(new AreaTypeKind(Long.valueOf(areaTypeKindCode)));
        }

        String areaTypeClassCode = getValue(pack, AreaType.FieldsEnum.AREA_TYPE_CLASS_CODE.toString());
        if (areaTypeClassCode.length() > 0) {
            areaType.setAreaTypeClass(new AreaTypeClass(Long.valueOf(areaTypeClassCode)));
        }

        String genderCode = getValue(pack, AreaType.FieldsEnum.GENDER_CODE.toString());
        if (genderCode.length() > 0) {
            areaType.setGender(genderCode);
        }

        String ageMin = getValue(pack, AreaType.FieldsEnum.AGE_MIN.toString());
        if (ageMin.length() > 0) {
            areaType.setAgeMin(Integer.valueOf(ageMin));
        }

        String ageMax = getValue(pack, AreaType.FieldsEnum.AGE_MAX.toString());
        if (ageMax.length() > 0) {
            areaType.setAgeMax(Integer.valueOf(ageMax));
        }

        String ageMMin = getValue(pack, AreaType.FieldsEnum.AGE_M_MIN.toString());
        if (ageMMin.length() > 0) {
            areaType.setAgeMMin(Integer.valueOf(ageMMin));
        }

        String ageMMax = getValue(pack, AreaType.FieldsEnum.AGE_M_MAX.toString());
        if (ageMMax.length() > 0) {
            areaType.setAgeMMax(Integer.valueOf(ageMMax));
        }

        String ageWMin = getValue(pack, AreaType.FieldsEnum.AGE_W_MIN.toString());
        if (ageWMin.length() > 0) {
            areaType.setAgeWMin(Integer.valueOf(ageWMin));
        }

        String ageWMax = getValue(pack, AreaType.FieldsEnum.AGE_W_MAX.toString());
        if (ageWMax.length() > 0) {
            areaType.setAgeWMax(Integer.valueOf(ageWMax));
        }

        String headFinance = getValue(pack, AreaType.FieldsEnum.HEAD_FINANCE.toString());
        if (headFinance.length() > 0) {
            areaType.setHeadFinance(stringIsTrue(headFinance));
        }

        String hasServiceTerritory = getValue(pack, AreaType.FieldsEnum.HAS_SERVICE_TERRITORY.toString());
        if (hasServiceTerritory.length() > 0) {
            areaType.setHasServiceTerritory(stringIsTrue(hasServiceTerritory));
        }

        String attachByRequest = getValue(pack, AreaType.FieldsEnum.ATTACH_BY_REQUEST.toString());
        if (attachByRequest.length() > 0) {
            areaType.setAttachByRequest(stringIsTrue(attachByRequest));
        }

        String attachByMedicalReason = getValue(pack, AreaType.FieldsEnum.ATTACH_BY_MEDICAL_REASON.toString());
        if (attachByMedicalReason.length() > 0) {
            areaType.setAttachByMedicalReason(stringIsTrue(attachByMedicalReason));
        }

        String archived = getValue(pack, AreaType.FieldsEnum.ARCHIVED.toString());
        if (archived.length() > 0) {
            areaType.setArchived(stringIsTrue(archived));
        }
        return areaType;
    }

    public static AreaTypeClass mapAreaTypeClass(Package pack) {

        AreaTypeClass areaTypeClass = new AreaTypeClass();

        String code = getValue(pack, AreaTypeClass.FieldsEnum.CODE.toString());
        if (code.length() > 0) {
            areaTypeClass.setCode(Long.valueOf(code));
        }

        String title = getValue(pack, AreaTypeClass.FieldsEnum.TITLE.toString());
        if (title.length() > 0) {
            areaTypeClass.setTitle(title);
        }

        String archived = getValue(pack, AreaTypeClass.FieldsEnum.ARCHIVED.toString());
        if (archived.length() > 0) {
            areaTypeClass.setArchived(stringIsTrue(archived));
        }
        return areaTypeClass;
    }

    public static AreaTypeKind mapAreaTypeKind(Package pack) {
        AreaTypeKind areaTypeKind = new AreaTypeKind();

        String code = getValue(pack, AreaTypeKind.FieldsEnum.CODE.toString());
        if (code.length() > 0) {
            areaTypeKind.setCode(Long.valueOf(code));
        }

        String title = getValue(pack, AreaTypeKind.FieldsEnum.TITLE.toString());
        if (title.length() > 0) {
            areaTypeKind.setTitle(title);
        }

        String archived = getValue(pack, AreaTypeKind.FieldsEnum.ARCHIVED.toString());
        if (archived.length() > 0) {
            areaTypeKind.setArchived(stringIsTrue(archived));
        }
        return areaTypeKind;
    }

    public static AreaTypeMedicalPositions mapAreaTypeMedicalPositions(Package pack) {
        AreaTypeMedicalPositions areaTypeMedicalPositions = new AreaTypeMedicalPositions();

        String id = getValue(pack, AreaTypeMedicalPositions.FieldsEnum.ID.toString());
        if (id.length() > 0) {
            areaTypeMedicalPositions.setId(Long.valueOf(id));
        }

        String areaTypeCode = getValue(pack, AreaTypeMedicalPositions.FieldsEnum.AREA_TYPE_CODE.toString());
        if (areaTypeCode.length() > 0) {
            areaTypeMedicalPositions.setAreaType(new AreaType(Long.valueOf(areaTypeCode)));
        }

        String positionCode = getValue(pack, AreaTypeMedicalPositions.FieldsEnum.POSITION_CODE.toString());
        if (positionCode.length() > 0) {
            areaTypeMedicalPositions.setPositionNomCode(positionCode);
        }

        String archived = getValue(pack, AreaTypeMedicalPositions.FieldsEnum.ARCHIVED.toString());
        if (archived.length() > 0) {
            areaTypeMedicalPositions.setArchived(stringIsTrue(archived));
        }
        return areaTypeMedicalPositions;
    }

    public static AreaTypeRelations mapAreaTypeRelations(Package pack) {
        AreaTypeRelations areaTypeRelations = new AreaTypeRelations();

        String id = getValue(pack, AreaTypeRelations.FieldsEnum.ID.toString());
        if (id.length() > 0) {
            areaTypeRelations.setId(Long.valueOf(id));
        }

        String dependentAreaTypeCode = getValue(pack, AreaTypeRelations.FieldsEnum.DEPENDENT_AREA_TYPE_CODE.toString());
        if (dependentAreaTypeCode.length() > 0) {
            areaTypeRelations.setDependentAreaType(new AreaType(Long.valueOf(dependentAreaTypeCode)));
        }

        String primaryAreaTypeCode = getValue(pack, AreaTypeRelations.FieldsEnum.PRIMARY_AREA_TYPE_CODE.toString());
        if (primaryAreaTypeCode.length() > 0) {
            areaTypeRelations.setPrimaryAreaType(new AreaType(Long.valueOf(primaryAreaTypeCode)));
        }

        String archived = getValue(pack, AreaTypeRelations.FieldsEnum.ARCHIVED.toString());
        if (archived.length() > 0) {
            areaTypeRelations.setArchived(stringIsTrue(archived));
        }
        return areaTypeRelations;
    }

    public static AreaTypeSpecializations mapAreaTypeSpecializations(Package pack) {
        AreaTypeSpecializations areaTypeSpecializations = new AreaTypeSpecializations();

        String id = getValue(pack, AreaTypeSpecializations.FieldsEnum.ID.toString());
        if (id.length() > 0) {
            areaTypeSpecializations.setId(Long.valueOf(id));
        }

        String areaTypeCode = getValue(pack, AreaTypeSpecializations.FieldsEnum.AREA_TYPE_CODE.toString());
        if (areaTypeCode.length() > 0) {
            areaTypeSpecializations.setAreaType(new AreaType(Long.valueOf(areaTypeCode)));
        }

        String specializationCode = getValue(pack, AreaTypeSpecializations.FieldsEnum.SPECIALIZATION_CODE.toString());
        if (specializationCode.length() > 0) {
            areaTypeSpecializations.setSpecializationCode(Long.valueOf(specializationCode));
        }

        String archived = getValue(pack, AreaTypeSpecializations.FieldsEnum.ARCHIVED.toString());
        if (archived.length() > 0) {
            areaTypeSpecializations.setArchived(stringIsTrue(archived));
        }
        return areaTypeSpecializations;
    }
}
