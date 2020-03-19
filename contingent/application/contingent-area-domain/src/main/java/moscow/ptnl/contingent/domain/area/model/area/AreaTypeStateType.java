package moscow.ptnl.contingent.domain.area.model.area;

public enum AreaTypeStateType {

    //Используемые в МУ
    USED_IN_MU(0),
    //1 - Доступные для добавления в список МУ
    AVAILABLE_TO_ADD(1),
    //2 - Все
    ALL(2);

    private int value;

    AreaTypeStateType(int value) {
        this.value = value;
    }

    public int getValue() {
        return value;
    }

    public static AreaTypeStateType getByValue(int value) {
        for (AreaTypeStateType areaTypeStateType : AreaTypeStateType.values()) {
            if (areaTypeStateType.value == value) {
                return areaTypeStateType;
            }
        }
        return null;
    }
}
