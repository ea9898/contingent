package moscow.ptnl.contingent.sysop;

import ru.mos.emias.errors.domain.ErrorMessageType;
import ru.mos.emias.errors.domain.ErrorReason;


/**
 * Пользовательские сообщения
 */
public enum SysopErrorReason implements ErrorReason {

    UNEXPECTED_ERROR("E000", "Произошла ошибка: %s"),
    OPERATION_NOT_FOUND("E001", "Операция с ИД: %s не найдена в системе")
    ;

    private final String description;
    private final String code;
    private final ErrorMessageType messageType;

    SysopErrorReason(String code, String description) {
        this(ErrorMessageType.ERROR, code, description);
    }
    
    SysopErrorReason(ErrorMessageType messageType, String code, String description) {
        this.messageType = messageType;
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

    @Override
    public ErrorMessageType getMessageType() {
        return this.messageType;
    }

}
