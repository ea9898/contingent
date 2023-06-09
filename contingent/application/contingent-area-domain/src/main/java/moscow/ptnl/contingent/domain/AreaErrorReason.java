package moscow.ptnl.contingent.domain;

import ru.mos.emias.errors.domain.ErrorMessageType;
import ru.mos.emias.errors.domain.ErrorReason;

/**
 * Пользовательские сообщения возвращаемые при работе с участками.
 */
public enum AreaErrorReason implements ErrorReason {

    UNEXPECTED_ERROR("E000", "Непредвиденная ошибка. %s"),
    AREA_TYPE_NOT_FOUND("Е002", "Тип участка с ИД: %s не найден в системе"),
    AREA_NOT_FOUND("E006", "Участок обслуживания МО с ИД %s не найден в системе"),

    FORBIDDEN_ADDRESS_UPDATE_FIELDS("E007", "Редактирование следующих параметров запрещено: %s"), //С_УУ_НСИ_7
    NSI_ADDRESS_NOT_FOUND("E008", "Адрес с переданным globalId %s не найден"), //С_УУ_НСИ_8
    WRONG_ADDRESS_UPDATE_FIELDS("E009", "Атрибуты %s не найдены"), //С_УУ_НСИ_9

    AREA_IS_ARCHIVED("E012", "Участок с ИД %s находится в статусе «Архивный»"),
    AREA_IS_NOT_ARCHIVED("E014", "Участок с ИД %s находится в статусе «Активный»"),
    ADDRESS_ALREADY_SERVICED_ANOTHER_AREA("E018", "Адрес %s уже обслуживается участком с ИД %s. Адрес, включенный в территорию обслуживания участка, не должен входить в территорию обслуживания другого участка с ИД типа участка %s"),
    NO_SEARCH_PARAMETERS("E024", "Не заданы критерии поиска"),
    AREA_TYPE_NOT_EXISTS_IN_MO("E026", "Тип участка %s отсутствует в списке доступных для МО"),
    AREA_TYPE_ALREADY_EXISTS("E027", "Тип участка %s уже присутствует в списке доступных"),
    AREA_TYPE_NOT_EXISTS_IN_MU("E028", "Тип участка %s отсутствует в списке доступных для МУ"),
    CANT_DELETE_AREA_TYPE("E030", "Невозможно удалить тип участка с ИД %s, так как он присутствует в списке доступных для МУ с ИД %s"),
    ADDRESS_ALLOCATION_ORDER_EXISTS("E031", "Распоряжение с указанными параметрами уже существует в системе"),
    ADDRESS_ALLOCATION_ORDER_NOT_EXISTS("E032", "Распоряжение с ИД %s не найдено в системе"),
    ADDRESS_ALLOCATION_ORDER_IS_ARCHIVED("E033", "Распоряжение с ИД %s находится в архиве"),
    ADDRESS_NOT_FOUND("Е037", "Адрес не найден"),
    MULTIPLE_NSI_ADDRESSES_ERROR("Е038", "По переданным параметрам найдено более одной записи. Ошибка НСИ."),
    MAX_AREA_IDS_EXCEEDED("E040", "Превышено максимальное допустимое количество запрашиваемых участков (не более %s)"),
    OPTIONS_KEY_IS_NOT_UNIQUE("E041", "Необходимо указать только 1 параметр %s"),
    INCORRECT_OPTIONS_KEY("E042", "Допустимые значения параметра key: %s"),
    INCORRECT_OPTIONS_VALUE("E043", "Допустимые значения параметра value: %s"),
    INCORRECT_SORTING_FIELD("E044", "Сортировка результатов возможна по полям: %s"),

    DATE_IN_INCORRECT_PERIOD("UE026", "Дата издания распоряжения не может быть меньше 01.01.1970 или больше текущей даты"),
    AREA_WITH_TYPE_AND_NUMBER_EXISTS_IN_MO("UE045", "Участок обслуживания с типом %s и номером %s уже существует в рамках филиала МО"),
    AREA_TYPE_NOT_AVAILABLE_FOR_MU("UE046", "Невозможно создать/изменить участок, так как тип участка %s отсутствует в списке разрешённых"),
    AREAS_NUMBER_LIMIT_EXCEEDED("UE047", "Невозможно создать участок, так как количество участков с типом %s превысит ограничение (%s)"),
    AREA_WITH_TYPE_EXISTS_IN_MO("UE048", "В МО/филиале МО уже существует участок с типом %s"),
    SOFT_RELATED_AREA_MUST_BE_FILLED("UE049", "Для участка вида «Мягко-ассоциированный участок» должны быть заполнены описание, номер, возрастной контингент"),
    POLICY_TYPE_IS_INCORRECT("UE050", "Невозможно создать/изменить участок, так как добавляемый тип полиса отличен от ОМС"),
    AREA_NOT_RELATED_TO_SPECIAL_OFFICE("UE051", "Изменение филиала возможно только для участка вида «Участок кабинета»"),
    AREA_NOT_RELATED_TO_MILDLY_ASSOCIATED("UE052", "Изменение основного медицинского работника возможно только для видов «Мягко-ассоциированный участок», «Участок кабинета»"),
    SPECIALIZATION_NOT_RELATED_TO_AREA("UE053", "Специализация %s исполнения должности с ИД %s не соответствует специализации типа участка %s"),
    POSITION_NOT_SET_FOR_AREA_TYPE("UE054", "Должность %s исполнения должности с ИД %s не задана для типа участка %s"),
    JOB_ID_DATE_OVERLAP("UE055", "Периоды назначения исполнения должности с ИД %s для участка с ИД %s пересекаются (%s - %s; %s - %s)"),
    MAIN_EMPLOYEE_DATE_OVERLAP("UE056", "Периоды назначения основных медработников с ИД исполнения должности (%s, %s) пересекаются"),
    START_DATE_IN_PAST("UE057", "Дата назначения на участок не может быть в прошлом (%s)"),
    INCORRECT_AREA_AGE_SETUPS("UE058", "Для участка должны быть заполнены только те возрастные ограничения, которые заполнены для соответствующего типа участка"),
    AREA_AGE_SETUP_EXCEEDED("UE059", "Возрастной контингент участка (%s; %s) выходит за пределы возрастного контингента типа участка (%s; %s)"),
    AREA_FLAGS_INCORRECT("UE060", "Для участка не может быть одновременно установлены признаки «Назначать для автоматического прикрепления» и «Необходимость медицинских показаний»"),
    ATTACH_BY_MEDICAL_REASON_INCORRECT("UE061", "Значение признака «Необходимость медицинских показаний» (%s) не соответствует настройкам типа участка (%s)"),
    EMPLOYEE_NOT_RELATED_TO_AREA("UE062", "Медработник с ИД назначения %s не назначен на участок"),
    START_DATE_IS_AFTER_END_DATE("UE063", "Дата окончания назначения на участок (%s) не может быть раньше даты начала назначения (%s)"),
    REPLACEMENT_WITHOUT_MAIN_EMPLOYEE("UE064", "В период (%s - %s) на участке будут назначены замещающие медработники без основного медработника"),
    CANT_SET_AUTO_ASSIGN_FOR_ATTACHMENT("UE065", "Невозможно установить признак для автоматического прикрепления, т.к. для данного типа участка (%s) не разрешено прикрепление через МПГУ"),
    NOTHING_TO_CHANGE("UE066", "Отсутствуют сведения для изменения"),
    AREA_IS_AUTO_ATTACH("UE067", "Участок с ИД %s назначен для автоматического прикрепления. Архивирование невозможно"),
    CANT_DELETE_EMPLOYEE("UE068", "Удаление медработника(-ов) невозможно, т.к. на участке не останется ни одного основного врача"),
    CANT_RESTORE_PERSONAL_KIND_AREA("UE069", "Восстановление из архива невозможно, т.к. вид участка «Именной»"),
    INCORRECT_ADDRESS_NESTING("UE070", "Некорректная вложенность адреса, не указано муниципальное образование"),
    AREA_TYPE_IS_NOT_PRIMARY("UE071", "Невозможно создать/изменить участок, т.к. тип участка %s не является типом участка первичного класса"),
    INCORRECT_ADDRESS_LEVEL("UE072", "Некорректный уровень адреса %s"),
    CODES_NOT_SET("UE073", "Для адреса с кодом %s и уровнем %s не переданы:"),
    AREA_TYPE_IS_NOT_DEPENDENT("UE074", "Невозможно создать/изменить участок, т.к. тип участка %s не является типом участка зависимого класса"),
    AO_LEVEL_NOT_SET("UE075", "Невозможно добавить адрес, т.к. не указан его уровень"),
    NOT_SINGLE_AREA_OR_REGION("UE076", "Для дома должны быть указаны только один район и округ Москвы"),
    AREA_TYPE_RELATIONS_NOT_EXISTS("UE077", "Не разрешена связь между типами участков %s и %s"),
    ADDRESS_ALREADY_EXISTS("UE078", "Адрес %s уже обслуживается МО с ИД %s"),
    TOO_MANY_ADDRESSES("UE079", "Превышено максимально допустимое количество адресов для распределения (не более %s)"),
    MO_ADDRESS_NOT_EXISTS("UE080", "Территория обслуживания с ИД %s не существует или уже перенесена в архив или не соответствует ИД участка"),
    TOO_BIG_PAGE_SIZE("UE081", "Размер страницы превышает максимально допустимое значение (не более %s)"),
    ADDRESS_NOT_SERVICED_MO_NSI("UE082", "Адрес %s не включен в территорию обслуживания МО %s."),
    ADDRESS_ALREADY_SERVICED_NSI("UE083", "Адрес %s уже обслуживается данным участком."),
    ADDRESS_ALREADY_SERVICED_NOTNSI("UE083", "Адрес ИД: %s, %s: %s %s уже обслуживается данным участком."),
    POLICY_TYPE_NOT_SET_FOR_AREA("UE084", "Тип полиса с кодом %s не задан для участка с ИД %s"),
    AREA_NOT_DEPEND_ON_AREA_TYPE("UE085", "Участок с ИД %s не зависит от первичного типа участка %s"),
    NO_MU_ID_PARAMETER("UE086", "Не указан филиал МО"),
    AREA_ALREADY_DEPENDS_ON_AREA_TYPE("UE087", "Участок с ИД %s уже зависит от первичного типа участка %s"),
    SEARCH_AREA_INACCURATE_ADDRESS_ERROR("UE088", "Поиск участков по неточному совпадению может быть выполнен только для одного адреса"),
    AREAS_NOT_IN_MO("UE089", "Участки с ИД %s не входят в состав МО с ИД %s"),
    EMPTY_AREA_TYPE_PROFILE("UE090", "Не указан код профиля (обязателен для типа участка %s)"),
    AREA_TYPE_PROFILE_NOT_FOUND("UE091", "Переданный профиль не найден в справочнике или не соответствует типу участка %s"),
    CANT_ADD_SERVICED_MU("UE092", "К типу участка %s не могут быть распределены медучреждения для обслуживания"),
    MU_ALREADY_SERVICED("UE093", "Медучреждение %s обслуживается участком %s с тем же профилем типа участка"),
    POSITION_CODE_NOT_FOUND("UE094", "Код должности медработника %s не найден в системе"),
    AREA_TYPE_IS_NOT_PRIMARY_2("UE095", "Тип участка с ИД: %s не является первичным"),
    AREA_TYPE_DO_NOT_SERVES_TERRITORY("UE096", "Тип участка с ИД: %s не имеет признак \"Обслуживает территорию\""),
    TOO_MANY_REQUEST_ADDRESSES("UE097", "Превышено максимально допустимое количество адресов для поиска (не более %s)"),
    NOT_REPLACEMENT_EMPLOYEE("UE098", "Медработник, ВРИО основного участкового врача, должен быть замещающим врачом на участке"),
    MAIN_EMPLOYEE_EXISTS("UE100", "МР %s уже является основным на участке %s. МР не может одновременно быть основным на двух и более участках одного типа"),
    MULTIPLE_TEMP_DUTY_EMPLOYEES("UE101", "На текущем участке уже есть медработник ВРИО основного участкового врача с ИД исполнения должности: %s"),
    TEMP_DUTY_EMPLOYEE_EXISTS("UE102", "Один и тот же МР %s не может быть ВРИО на 2-х и более участках одного и того же типа. Сейчас он ВРИО на участке %s"),
    MU_SERVICE_NOT_FOUND("UE103", "Не найдено обслуживаемых МУ в рамках выбранного типа участка"),
    PAGING_INCORRECT("UE106", "Указано некорректное значение параметра пагинации"),
    SERVICING_MU_NOT_FOUND("UE107", "Не найдено специализированных МО/МУ в рамках выбранного типа участка"),
    SPECIALIZATION_IS_NOT_SPECIFIED("UE111", "Для должности мед. работника в справочнике номенклатуры должностей не указана специализация"),
    SPECIALIZATION_CODE_IS_NOT_DEFINED("UE112", "Для ИД специализации мед. работника %s не определён код специализации или он заархивирован"),
    ADDRESSES_WITH_GLOBAL_ID_NOT_FOUND_IN_NSI2("UE113", "Адрес с global_id %s не найден в НСИ.2"),
    IT_IS_ALLOWED_ENTER_ONE_MORE_THAN_ONE_ADDRESS("UE114", "Для поиска участка по адресообразующему элементу допускается вводить не более одного адреса"),
    SEARCH_AREA_NOT_MOSCOW_ADDRESS("UE115", "Указан адрес не в московском регионе")
    ;

    private final String description;
    private final String code;
    private final ErrorMessageType messageType;

    AreaErrorReason(String code, String description) {
        this(code, description, ErrorMessageType.ERROR);
    }
    
    AreaErrorReason(String code, String description, ErrorMessageType messageType) {
        this.code = code;
        this.description = description;
        this.messageType = messageType;
    }

    @Override
    public String getDescription() {
        return description;
    }
    @Override
    public String getCode() {
        return code;
    }

    @Override
    public ErrorMessageType getMessageType() {
        return messageType;
    }

}
