package moscow.ptnl.contingent.error;

import ru.mos.emias.errors.domain.ErrorMessageType;
import ru.mos.emias.errors.domain.ErrorReason;

/**
 * Пользовательское сообщение.
 * 
 * @author m.kachalov
 */
public class CustomErrorReason implements ErrorReason {
    
    private final String description;
    private final String code;
    private final ErrorMessageType messageType;
    
    public CustomErrorReason(String description, String code) {
        this(description, code, ErrorMessageType.ERROR);
    }
    
    public CustomErrorReason(String description, String code, ErrorMessageType messageType) {
        this.description = description;
        this.code = code;
        this.messageType = messageType;
    }

    @Override
    public String getDescription() {
        return this.description;
    }

    @Override
    public String getCode() {
        return this.code;
    }

    @Override
    public ErrorMessageType getMessageType() {
        return this.messageType;
    }
    
}
