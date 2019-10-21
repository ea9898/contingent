package moscow.ptnl.contingent.nsi.error;

import ru.mos.emias.errors.domain.ErrorMessageType;
import ru.mos.emias.errors.domain.ErrorReason;


public enum NsiEhdErrorReason implements ErrorReason {

    UNEXPECTED_ERROR("E000", "Произошла ошибка: %s"),
    CATALOG_ID_NOT_FOUND("E001", "Для каталога с наименованием %s на найден ИД для обращения с СУ НСИ."),
    CANNOT_UPDATE_DICT("E002", "Данные справочника не получены"),
    UPDATE_DICT_ERROR("E003", "Ошибка обновления справочника: %s");

    private final String description;
    private final String code;
    private final ErrorMessageType messageType;
    
    NsiEhdErrorReason(String code, String description) {
        this(code, description, ErrorMessageType.ERROR);
    }

    NsiEhdErrorReason(String code, String description, ErrorMessageType messageType) {
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
