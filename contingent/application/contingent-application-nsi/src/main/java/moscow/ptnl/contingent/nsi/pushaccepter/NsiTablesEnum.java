package moscow.ptnl.contingent.nsi.pushaccepter;

import java.util.Arrays;

public enum NsiTablesEnum {
    AREA_TYPE("catalog_1146");

    private final String name;

    NsiTablesEnum(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }

    public static NsiTablesEnum getByName(String name) {
        return Arrays.stream(NsiTablesEnum.values()).filter(e -> e.getName().equals(name)).findFirst()
                .orElseThrow(() -> new IllegalStateException(String.format("Нет такой таблицы: %s", name)));
    }
}