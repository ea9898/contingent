package moscow.ptnl.contingent.nsi.pushaccepter;

import moscow.ptnl.contingent.area.entity.nsi.AreaType;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeClass;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeKind;
import moscow.ptnl.contingent.nsi.pushaccepter.xmlparsingS.Table;
import moscow.ptnl.contingent.nsi.pushaccepter.xmlparsing.Package;
import moscow.ptnl.contingent.repository.nsi.AreaTypesCRUDRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class PushAccepterImpl extends PushAccepter {

    @Autowired
    AreaTypesCRUDRepository areaTypesCRUDRepository;

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
                AreaType areaType = map(pack);
                if (pack.catalog.data.action.equalsIgnoreCase(NsiActionsEnum.DELETED.toString())) {
                    areaTypesCRUDRepository.delete(areaType);
                } else {
                    areaTypesCRUDRepository.save(areaType);
                }
        }
        return new Answer(true, "ok");
    }

    private String getValue(Package pack, String parameterName) {
        return pack.catalog.data.attribute.stream().filter(atr -> atr.name.equalsIgnoreCase(parameterName)).findFirst()
                .orElseThrow(() -> new IllegalStateException(String.format("Нет такого поля %s в таблице %s", parameterName, pack.catalog.name)))
                .values.value.value;
    }

    private boolean stringIsTrue(String booleanValue) {
        return booleanValue.equals("1");
    }

    private AreaType map(Package pack) {
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
}
