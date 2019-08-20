package moscow.ptnl.contingent.domain.nsi;

import java.util.Arrays;

public enum NsiTablesEnum {
    
    UNKNOWN(null),
    AREA_TYPE(1146),
    AREA_TYPE_CLASS(1142),
    AREA_TYPE_KIND(1144),
    AREA_TYPE_MEDICAL_POSITIONS(1154),
    AREA_TYPE_RELATIONS(1150),
    AREA_TYPE_SPECIALIZATIONS(1148);

    private final Integer code;

    NsiTablesEnum(Integer name) {
        this.code = name;
    }

    public Integer getCode() {
        return code;
    }

    public static NsiTablesEnum getByName(String name) {
        return Arrays.stream(NsiTablesEnum.values()).filter(e -> e.getCode() != null && ("catalog_" + e.getCode()).equals(name)).findFirst()
                .orElseThrow(() -> new IllegalStateException(String.format("Таблица %s не поддерживается", name)));
    }

    public static NsiTablesEnum getByCode(Integer code) {
        return Arrays.stream(NsiTablesEnum.values()).filter(e -> e.getCode() != null && e.getCode().equals(code)).findFirst()
                .orElseThrow(() -> new IllegalStateException(String.format("Таблица %s не поддерживается", code)));
    }
}