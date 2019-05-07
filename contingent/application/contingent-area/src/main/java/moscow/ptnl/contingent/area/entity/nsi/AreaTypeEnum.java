package moscow.ptnl.contingent.area.entity.nsi;

/**
 * Пользовательские сообщения возвращаемые при работе с участками
 */
public enum AreaTypeEnum {

    MILDLY_ASSOCIATED(1),
    TREATMENT_ROOM_ASSOCIATED(2);

    private final long code;

    AreaTypeEnum(long code) {
        this.code = code;
    }

    public long getCode() {
        return code;
    }

}
