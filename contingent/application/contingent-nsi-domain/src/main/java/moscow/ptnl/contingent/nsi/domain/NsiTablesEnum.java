package moscow.ptnl.contingent.nsi.domain;

import java.util.Arrays;

public enum NsiTablesEnum {
    
    UNKNOWN(-1),
    AREA_TYPE(1146),
    AREA_TYPE_CLASS(1142),
    AREA_TYPE_KIND(1144),
    AREA_TYPE_MEDICAL_POSITIONS(1154),
    AREA_TYPE_RELATIONS(1150),
    AREA_TYPE_SPECIALIZATIONS(1148),
    SPECIALIZATION(1115),
    POSITION_CODE(1128),
    GENDER(1122),
    D_POSITION_NOM(1129),
    POLICY_TYPE(1140),
    AREA_TYPE_PROFILE(2416),
    POSITION_SUPP(1381),
    MAPPING_POSITIONCODE_TO_OTHERPOSITION(1382);

    private final Integer code;

    NsiTablesEnum(Integer name) {
        this.code = name;
    }

    public Integer getCode() {
        return code;
    }

    public static NsiTablesEnum getByName(String name) {
        return NsiTablesEnum.valueOf(name);
    }

    public static NsiTablesEnum getByCode(Integer code) {
        return Arrays.stream(NsiTablesEnum.values()).filter(e -> e.getCode() != null && e.getCode().equals(code)).findFirst()
                .orElse(NsiTablesEnum.UNKNOWN);
    }

    public static NsiTablesEnum getByCatalogName(String name) {
        return getByCode(Integer.parseInt(Arrays.asList(name.split("_")).get(1)));
    }
}
