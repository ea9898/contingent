package moscow.ptnl.contingent.area.error;

/**
 * Пользовательские сообщения возвращаемые при работе с участками
 */
public enum AreaErrorReason implements ErrorReason {

    UNEXPECTED_ERROR("E000", "Непредвиденная ошибка. %s"),
    AREA_TYPE_NOT_FOUND("Е002", "Тип участка с ИД: %s не найден в системе"),
    AREA_NOT_FOUND("E006", "Участок обслуживания МО с ИД %s не найден в системе"),
    AREA_IS_ARCHIVED("E012", "Участок с ИД %s находится в статусе «Архивный»"),
    NO_SEARCH_PARAMETERS("E024", "Не заданы критерии поиска"),
    AREA_TYPES_NOT_EXISTS_IN_PROFILE("E026", "Тип(ы) участка с ИД %s отсутствует(ют) в профиле"),
    MU_PROFILE_EXISTS("E027", "Для МУ %s участок с типом %s уже задан"),
    CANT_CHANGE_AREA_TYPE("E028", "Невозможно изменить тип участка %s. В шаблоне профиля допустимость создания данного типа отлична от «Возможно»"),
    CANT_DELETE_AREA_TYPE("E030", "Невозможно удалить тип участка %s, так как существуют активные участки данного типа"),
    ADDRESS_ALLOCATION_ORDER_EXISTS("E031", "Распоряжение с указанными параметрами уже существует в системе"),
    ADDRESS_ALLOCATION_ORDER_NOT_EXISTS("E032", "Распоряжение с ИД %s не найдено в системе"),
    ADDRESS_ALLOCATION_ORDER_IS_ARCHIVED("E033", "Распоряжение с ИД %s находится в архиве"),

    DATE_IN_INCORRECT_PERIOD("UE026", "Дата издания распоряжения не может быть меньше 01.01.1970 или больше текущей даты"),
    AREA_WITH_TYPE_AND_NUMBER_EXISTS_IN_MO("UE045", "Участок обслуживания с типом %s и номером %s уже существует в рамках филиала МО"),
    MU_PROFILE_HAS_NO_AREA_TYPE("UE046", "Невозможно создать/изменить участок, так как тип участка %s отсутствует в профиле МУ"),
    AREAS_NUMBER_LIMIT_EXCEEDED("UE047", "Невозможно создать участок, так как количество участков с типом %s превысит ограничение (%s)"),
    AREA_WITH_TYPE_EXISTS_IN_MO("UE048", "В МО уже существует участок с типом %s"),
    SOFT_RELATED_AREA_MUST_BE_FILLED("UE049", "Для участка вида «Мягко-ассоциированный участок» должны быть заполнены описание, номер, возрастной контингент"),
    NO_PRIMARY_AREA("UE050", "Невозможно создать/изменить участок, так как отсутствует участок первичного типа (%s)"),
    AREA_NOT_RELATED_TO_SPECIAL_OFFICE("UE051", "Изменение филиала возможно только для участка вида «Ассоциированный со специализированным кабинетом»"),
    AREA_NOT_RELATED_TO_MILDLY_ASSOCIATED_OR_TREATMENT_ROOM_ASSOCIATED("UE052", "Изменение основного медицинского работника возможно только для видов «Мягко-ассоциированный участок», «Ассоциированный со специализированным кабинетом»"),
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
