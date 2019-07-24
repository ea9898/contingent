package moscow.ptnl.contingent.area.entity.nsi;

/**
 * Вид участка
 */
//TODO коды заданы произвольно, после заполнения справочника area_types необходимо скорректировать
public enum AreaTypeKindEnum {

    //Мягко-ассоциированный участок
    MILDLY_ASSOCIATED(1),
    //Обезличенный участок
    DEPERSONALIZED(2),
    //Участок кабинета
    TREATMENT_ROOM_ASSOCIATED(3),
    //Специализированный именной  участок
    PERSONAL(4);

    private final long code;

    AreaTypeKindEnum(long code) {
        this.code = code;
    }

    public long getCode() {
        return code;
    }

    public boolean equalsCode(Long code) {
        return code != null && this.code == code;
    }
}
