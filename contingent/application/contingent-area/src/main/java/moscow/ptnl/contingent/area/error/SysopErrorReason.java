package moscow.ptnl.contingent.area.error;

import moscow.ptnl.contingent.error.ErrorReason;

/**
 * Пользовательские сообщения
 */
public enum SysopErrorReason implements ErrorReason {

    UNEXPECTED_ERROR("E000", "Произошла ошибка: %s"),
    OPERATION_NOT_FOUND("E001", "Операция с ИД: %s не найдена в системе")
    ;

    private final String description;
    private final String code;

    SysopErrorReason(String code, String description) {
        this.code = code;
        this.description = description;
    }

    @Override
    public String getDescription() {
        return description;
    }

    @Override
    public String getCode() {
        return code;
    }

}
