package moscow.ptnl.contingent.area.entity.nsi;

/**
 * Вид участка
 */
public enum KindAreaTypeEnum {

    MILDLY_ASSOCIATED(1),
    TREATMENT_ROOM_ASSOCIATED(2),
    PERSONAL(3),
    DEPERSONALIZED(4);

    private final long code;

    KindAreaTypeEnum(long code) {
        this.code = code;
    }

    public long getCode() {
        return code;
    }

}
