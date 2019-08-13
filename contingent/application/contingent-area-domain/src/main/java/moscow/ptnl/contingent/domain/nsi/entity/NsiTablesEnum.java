package moscow.ptnl.contingent.domain.nsi.entity;

import java.util.Arrays;

public enum NsiTablesEnum {
    AREA_TYPE("catalog_1146"),
    AREA_TYPE_CLASS("catalog_1142"),
    AREA_TYPE_KIND("catalog_1144"),
    AREA_TYPE_MEDICAL_POSITIONS("catalog_1154"),
    AREA_TYPE_RELATIONS("catalog_1150"),
    AREA_TYPE_SPECIALIZATIONS("catalog_1148");

    private final String name;

    NsiTablesEnum(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }

    public static NsiTablesEnum getByName(String name) {
        return Arrays.stream(NsiTablesEnum.values()).filter(e -> e.getName().equals(name)).findFirst()
                .orElseThrow(() -> new IllegalStateException(String.format("Таблица %s не поддерживается", name)));
    }
}