package moscow.ptnl.contingent.nsi.pushaccepter;

import moscow.ptnl.contingent.area.entity.nsi.AreaType;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeClass;
import moscow.ptnl.contingent.domain.nsi.NsiTablesEnum;
import ru.mos.emias.nsiproduct.core.v1.EhdCatalogRow;

import java.util.List;
import java.util.stream.Collectors;

public class NsiEntityMapper {

    private static String getValue(EhdCatalogRow row, String parameterName, Integer tableCode) {
        return (String) row.getItem().stream().filter(atr -> atr.getTehName().equalsIgnoreCase(parameterName))
                .findFirst().orElseThrow(() -> new IllegalStateException(
                        String.format("Нет такого поля %s в таблице %s", parameterName, tableCode))).getValue();
    }

    private static boolean stringIsTrue(String booleanValue) {
        return booleanValue.equals("1");
    }

    public static List<AreaType> mapAreaTypes(List<EhdCatalogRow> rows) {

        return rows.stream().map(NsiEntityMapper::mapAreaType).collect(Collectors.toList());
    }

    public static AreaType mapAreaType(EhdCatalogRow row) {

        AreaType areaType = new AreaType();

        String code = getValue(row, AreaType.FieldsEnum.CODE.toString(), NsiTablesEnum.AREA_TYPE.getCode());
        if (code.length() > 0) {
            areaType.setCode(Long.valueOf(code));
        }

/*        String title = getValue(row, AreaType.FieldsEnum.TITLE.toString());
        if (title.length() > 0) {
            areaType.setTitle(title);
        }

        String areaTypeKindCode = getValue(row, AreaType.FieldsEnum.AREA_TYPE_KIND_CODE.toString());
        if (areaTypeKindCode.length() > 0) {
            areaType.setAreaTypeKind(new AreaTypeKind(Long.valueOf(areaTypeKindCode)));
        }

        String areaTypeClassCode = getValue(row, AreaType.FieldsEnum.AREA_TYPE_CLASS_CODE.toString());
        if (areaTypeClassCode.length() > 0) {
            areaType.setAreaTypeClass(new AreaTypeClass(Long.valueOf(areaTypeClassCode)));
        }

        String genderCode = getValue(row, AreaType.FieldsEnum.GENDER_CODE.toString());
        if (genderCode.length() > 0) {
            areaType.setGender(genderCode);
        }

        String ageMin = getValue(row, AreaType.FieldsEnum.AGE_MIN.toString());
        if (ageMin.length() > 0) {
            areaType.setAgeMin(Integer.valueOf(ageMin));
        }

        String ageMax = getValue(row, AreaType.FieldsEnum.AGE_MAX.toString());
        if (ageMax.length() > 0) {
            areaType.setAgeMax(Integer.valueOf(ageMax));
        }

        String ageMMin = getValue(row, AreaType.FieldsEnum.AGE_M_MIN.toString());
        if (ageMMin.length() > 0) {
            areaType.setAgeMMin(Integer.valueOf(ageMMin));
        }

        String ageMMax = getValue(row, AreaType.FieldsEnum.AGE_M_MAX.toString());
        if (ageMMax.length() > 0) {
            areaType.setAgeMMax(Integer.valueOf(ageMMax));
        }

        String ageWMin = getValue(row, AreaType.FieldsEnum.AGE_W_MIN.toString());
        if (ageWMin.length() > 0) {
            areaType.setAgeWMin(Integer.valueOf(ageWMin));
        }

        String ageWMax = getValue(row, AreaType.FieldsEnum.AGE_W_MAX.toString());
        if (ageWMax.length() > 0) {
            areaType.setAgeWMax(Integer.valueOf(ageWMax));
        }

        String headFinance = getValue(row, AreaType.FieldsEnum.HEAD_FINANCE.toString());
        if (headFinance.length() > 0) {
            areaType.setHeadFinance(stringIsTrue(headFinance));
        }

        String hasServiceTerritory = getValue(row, AreaType.FieldsEnum.HAS_SERVICE_TERRITORY.toString());
        if (hasServiceTerritory.length() > 0) {
            areaType.setHasServiceTerritory(stringIsTrue(hasServiceTerritory));
        }

        String attachByRequest = getValue(row, AreaType.FieldsEnum.ATTACH_BY_REQUEST.toString());
        if (attachByRequest.length() > 0) {
            areaType.setAttachByRequest(stringIsTrue(attachByRequest));
        }

        String attachByMedicalReason = getValue(row, AreaType.FieldsEnum.ATTACH_BY_MEDICAL_REASON.toString());
        if (attachByMedicalReason.length() > 0) {
            areaType.setAttachByMedicalReason(stringIsTrue(attachByMedicalReason));
        }

        String archived = getValue(row, AreaType.FieldsEnum.ARCHIVED.toString());
        if (archived.length() > 0) {
            areaType.setArchived(stringIsTrue(archived));
        }*/
        return areaType;
    }

    public static List<AreaTypeClass> mapAreaTypeClasses(List<EhdCatalogRow> rows) {

        return rows.stream().map(NsiEntityMapper::mapAreaTypeClass).collect(Collectors.toList());
    }

    public static AreaTypeClass mapAreaTypeClass(EhdCatalogRow row) {

        AreaTypeClass areaTypeClass = new AreaTypeClass();

        String code = getValue(row, AreaTypeClass.FieldsEnum.CODE.toString(), NsiTablesEnum.AREA_TYPE_CLASS.getCode());
        if (code.length() > 0) {
            areaTypeClass.setCode(Long.valueOf(code));
        }

        String title = getValue(row, AreaTypeClass.FieldsEnum.TITLE.toString(), NsiTablesEnum.AREA_TYPE_CLASS.getCode());
        if (title.length() > 0) {
            areaTypeClass.setTitle(title);
        }

        String archived = getValue(row, AreaTypeClass.FieldsEnum.ARCHIVED.toString(), NsiTablesEnum.AREA_TYPE_CLASS.getCode());
        if (archived.length() > 0) {
            areaTypeClass.setArchived(stringIsTrue(archived));
        }
        return areaTypeClass;
    }
}
