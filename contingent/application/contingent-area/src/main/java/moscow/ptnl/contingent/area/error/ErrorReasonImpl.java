package moscow.ptnl.contingent.area.error;

import moscow.ptnl.contingent.error.ErrorReason;

/**
 * Пользовательское сообщение возвращаемое при работе с участками
 */
public class ErrorReasonImpl implements ErrorReason {

    private String description;
    private String code;

    public ErrorReasonImpl(String description, String code) {
        this.description = description;
        this.code = code;
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
