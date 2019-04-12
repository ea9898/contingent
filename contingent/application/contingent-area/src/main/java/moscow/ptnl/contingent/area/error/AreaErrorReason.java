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
    CANT_DELETE_AREA_TYPE("E030", "Невозможно удалить тип участка %s, так как существуют активные участки данного типа"),

    AREA_WITH_TYPE_AND_NUMBER_EXISTS_IN_MO("UE045", "Участок обслуживания с типом %s и номером %s уже существует в рамках филиала МО"),
    MU_PROFILE_HAS_NO_AREA_TYPE("UE046", "Невозможно создать/изменить участок, так как тип участка %s отсутствует в профиле МУ"),
    AREAS_NUMBER_LIMIT_EXCEEDED("UE047", "Невозможно создать участок, так как количество участков с типом %s превысит ограничение (%s)"),
    AREA_WITH_TYPE_EXISTS_IN_MO("UE048", "В МО уже существует участок с типом %s"),
    SOFT_RELATED_AREA_MUST_BE_FILLED("UE049", "Для участка вида «Мягко-ассоциированный участок» должны быть заполнены наименование, номер, возрастной контингент"),
    NO_PRIMARY_AREA("UE050", "Невозможно создать/изменить участок, так как отсутствует участок первичного типа (%s)"),
    INCORRECT_AREA_AGE_SETUPS("UE058", "Для участка должны быть заполнены только те возрастные ограничения, которые заполнены для соответствующего типа участка"),
    AREA_AGE_SETUP_EXCEEDED("UE059", "Возрастной контингент участка (%s; %s) выходит за пределы возрастного контингента типа участка (%s; %s)"),
    AREA_FLAGS_INCORRECT("UE060", "Для участка не может быть одновременно установлены признаки «Назначать для автоматического прикрепления» и «Необходимость медицинских показаний»"),
    ATTACH_BY_MEDICAL_REASON_INCORRECT("UE061", "Значение признака «Необходимость медицинских показаний» (%s) не соответствует настройкам типа участка (%s)"),
    CANT_SET_AUTO_ASSIGN_FOR_ATTACHMENT("UE065", "Невозможно установить признак для автоматического прикрепления, т.к. для данного типа участка (%s) не разрешено прикрепление через МПГУ"),
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
