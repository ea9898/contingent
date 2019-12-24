package moscow.ptnl.contingent.attachment;

import ru.mos.emias.errors.domain.ErrorMessageType;
import ru.mos.emias.errors.domain.ErrorReason;

/**
 * Пользовательские сообщения возвращаемые при работе с участками.
 */
public enum AttachmentErrorReason implements ErrorReason {

    UNEXPECTED_ERROR("E000", "Непредвиденная ошибка. %s"),
    NEED_JOB_ID("UE001", "Необходимо указать идентификатор исполнения должности медработника"),
    AREA_FOR_CREATE_NOT_FOUND("UE002", "Не найдено ни одного участка для создания прикрепления"),
    TOO_MANY_AREA_FOR_CREATE("UE003", "Найдено более 1 участка для создания прикрепления"),
    AREA_FOR_CLOSE_NOT_FOUND("UE004", "Не найдено ни одного участка для закрытия прикрепления"),
    TOO_MANY_AREA_FOR_CLOSE("UE005", "Найдено более 1 участка для закрытия прикрепления")
    ;

    private final String description;
    private final String code;
    private final ErrorMessageType messageType;

    AttachmentErrorReason(String code, String description) {
        this(code, description, ErrorMessageType.ERROR);
    }

    AttachmentErrorReason(String code, String description, ErrorMessageType messageType) {
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
