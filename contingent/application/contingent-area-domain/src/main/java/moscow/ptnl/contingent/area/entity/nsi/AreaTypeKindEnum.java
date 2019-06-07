package moscow.ptnl.contingent.area.entity.nsi;

/**
 * Вид участка
 */
//TODO коды заданы произвольно, после заполнения справочника area_types необходимо скорректировать
public enum AreaTypeKindEnum {

    MILDLY_ASSOCIATED(1),
    TREATMENT_ROOM_ASSOCIATED(2),
    PERSONAL(3),
    DEPERSONALIZED(4);

    private final long code;

    AreaTypeKindEnum(long code) {
        this.code = code;
    }

    public long getCode() {
        return code;
    }

}
