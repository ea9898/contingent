package moscow.ptnl.contingent.domain.area.model.nsi;

import java.util.Arrays;

public enum AvailableToCreateType {

    POSSIBLE(1),
    ALLOWED(2);

    private Integer value;

    AvailableToCreateType(Integer value) {
        this.value = value;
    }

    public Integer getValue() {
        return value;
    }

    public static AvailableToCreateType find(int level) {
        return Arrays.stream(AvailableToCreateType.values())
                .filter(t -> t.value.equals(level))
                .findFirst().orElse(null);
    }
}
