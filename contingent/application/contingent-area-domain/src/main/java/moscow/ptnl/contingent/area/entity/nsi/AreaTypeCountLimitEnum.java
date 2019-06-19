package moscow.ptnl.contingent.area.entity.nsi;

/**
 * Ограничения по количеству участков в МО/МУ
 */

public enum AreaTypeCountLimitEnum {

    MO(2, 1, "Не более одного на МО"),
    MU(3, 1, "Не более одного на МУ");

    private final int code;
    private final int limit;
    private final String description;

    AreaTypeCountLimitEnum(int code, int limit, String description) {
        this.code = code;
        this.limit = limit;
        this.description = description;
    }

    public int getCode() {
        return code;
    }

    public int getLimit() {
        return limit;
    }
}
