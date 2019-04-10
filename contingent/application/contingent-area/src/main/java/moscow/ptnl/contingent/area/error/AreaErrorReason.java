package moscow.ptnl.contingent.area.error;

/**
 * Пользовательские сообщения возвращаемые при работе с участками
 */
public enum AreaErrorReason implements ErrorReason {

    UNEXPECTED_ERROR("E000", "Непредвиденная ошибка. %s"),
    AREA_TYPE_NOT_FOUND("Е002", "Тип участка с ИД: %s не найден в системе"),
    NO_INFO_TO_CHANGE("E026", "Не переданы сведения для изменения"),
    MU_PROFILE_EXISTS("E027", "Для МУ %s участок с типом %s уже задан"),
    CANT_CHANGE_AREA_TYPE("E028", "Невозможно изменить тип участка %s. В шаблоне профиля допустимость создания данного типа отлична от «Возможно»"),
    CANT_DELETE_AREA_TYPE("E030", "Невозможно удалить тип участка %s, так как существуют активные участки данного типа")
    ;

    private final String description;
    private final String code;

    AreaErrorReason(String code, String description) {
        this.code = code;
        this.description = description;
    }

    public String getDescription() {
        return description;
    }
    public String getCode() {
        return code;
    }

}
