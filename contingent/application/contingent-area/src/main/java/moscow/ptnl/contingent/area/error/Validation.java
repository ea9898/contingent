package moscow.ptnl.contingent.area.error;

import com.google.common.base.Preconditions;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Результат выполнения валидации
 */
public class Validation {

    /**
     * Результат проверки
     */
    boolean success;
    /**
     * Перечень ошибок
     */
    List<ValidationMessage> messages = new ArrayList<>();

    public Validation() { success = true; }

    /**
     * Добавить сообщение
     * @param message
     */
    private void addMessage(ValidationMessage message){
        Preconditions.checkNotNull(message);
        messages.add(message);

        List<String> params = new ArrayList<>();
        if (message.parameters != null)
            params.addAll(message.parameters.stream().map(c -> c.value).collect(Collectors.toList()));

        if (params.size() > 0)
            message.setMessage(String.format(message.getMessage(), params.toArray()));
        success = success && message.type != ValidationMessageType.ERROR;
    }

    public Validation merge(Validation b){
        b.messages.forEach(this::addMessage);
        return this;
    }

    public static Validation merge(Validation a, Validation b){
        Validation result = new Validation();
        a.messages.forEach(result::addMessage);
        b.messages.forEach(result::addMessage);
        return result;
    }

    public Validation error(ErrorReason reason, ValidationParameter... parameters){
        addMessage(new ValidationMessage(reason, ValidationMessageType.ERROR).addParameter(parameters));
        return this;
    }

    public Validation warning(ErrorReason reason, ValidationParameter... parameters){
        addMessage(new ValidationMessage(reason, ValidationMessageType.WARNING).addParameter(parameters));
        return this;
    }

    public boolean isSuccess() {
        return success;
    }

    public void setSuccess(boolean success) {
        this.success = success;
    }

    public List<ValidationMessage> getMessages() {
        return messages;
    }

    public void setMessages(List<ValidationMessage> messages) {
        this.messages = messages;
    }
}