package moscow.ptnl.contingent.area.error;


import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Результат валидации
 */
public class ValidationMessage {
    /**
     * Код сообщения
     */
    private String code;

    /**
     * Сообщение или параметры
     */
    private String message;
    /**
     * Тип сообщения
     */
    private ValidationMessageType type;

    /**
     * Параметры валидации
     */
    private final List<ValidationParameter> parameters = new ArrayList<>();

    public ValidationMessage() {
    }

    public ValidationMessage(ErrorReason errorReason, ValidationMessageType type) {
        if (errorReason == null) {
            throw new IllegalArgumentException("errorReason не может быть null");
        }
        if (type == null) {
            throw new IllegalArgumentException("type не может быть null");
        }
        this.code = errorReason.getCode();
        this.message = errorReason.getDescription();
        this.type = type;
    }

    public ValidationMessage addParameter(ValidationParameter... params) {
        if (params == null)
            return this;
        Collections.addAll(parameters, params);
        return this;
    }

    public String getCode() {
        return code;
    }

    public void setCode(String code) {
        this.code = code;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }

    public ValidationMessageType getType() {
        return type;
    }

    public void setType(ValidationMessageType type) {
        this.type = type;
    }

    public List<ValidationParameter> getParameters() {
        return parameters;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        ValidationMessage that = (ValidationMessage) o;

        if (code != null ? !code.equals(that.code) : that.code != null) return false;
        if (message != null ? !message.equals(that.message) : that.message != null) return false;
        if (type != that.type) return false;
        return !(parameters != null ? !parameters.equals(that.parameters) : that.parameters != null);

    }

    @Override
    public int hashCode() {
        int result = code != null ? code.hashCode() : 0;
        result = 31 * result + (message != null ? message.hashCode() : 0);
        result = 31 * result + (type != null ? type.hashCode() : 0);
        result = 31 * result + (parameters != null ? parameters.hashCode() : 0);
        return result;
    }

    @Override
    public String toString() {
        return String.format("%s  %s \n", code, message);
    }
}