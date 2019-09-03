package moscow.ptnl.contingent.area.model.area;

import java.util.Arrays;

public enum AddressLevelType {

    ID("8"),
    STREET("7"),
    PLAN("65"),
    PLACE("6"),
    CITY("4"),
    AREA("3"),
    AREA_TE("25"),
    REGION_TE("2"),
    MOSCOW("1");

    private String level;

    AddressLevelType(String level) {
        this.level = level;
    }

    public String getLevel() {
        return level;
    }

    public static AddressLevelType find(int level) {
        return Arrays.stream(AddressLevelType.values())
                .filter(t -> t.level.equals(level))
                .findFirst().orElse(null);
    }
}
