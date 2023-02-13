package moscow.ptnl.contingent.esu.producer;

import org.slf4j.Logger;
import ru.mos.emias.esu.lib.producer.EsuProducer;
import ru.mos.emias.esu.lib.producer.MessageMetadata;

import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.ExecutionException;

/**
 *
 * @author m.kachalov
 */
public interface ProducerService {
    
    EsuProducer getEsuProducer();
    
    Integer getCountResendLimit();
    
    Logger getLogger();
    
    /**
     * 
     * @return мсек
     */
    Integer getSendTimeInterval();
    
    default Optional<ErrorObject> publishMessage(String publishTopic, String message) {
        Objects.requireNonNull(publishTopic, "Имя топика не может быть null");
        Objects.requireNonNull(message, "Сообщение не может быть null");
        
        ErrorObject error = null;

        for (int attempt = 0; attempt < getCountResendLimit(); attempt++) {
            
            if (attempt > 0) {
                getLogger().info("Попытка [" + attempt + "] повторной отправки сообщения в топик [" + publishTopic + "]");
            }
            
            try {
                MessageMetadata result = getEsuProducer().publish(publishTopic, message);
                getLogger().debug("Сообщение отправлено в топик [" + publishTopic + "]" );
                return Optional.empty();
            } catch (InterruptedException | ExecutionException ex) {
                error = new ErrorObject("Sending error: " + ex.getCause().getMessage(), ex, attempt + 1);
            } catch (Exception ex) {
                error = new ErrorObject("Unexpected sending error: " + ex.getMessage(), ex, attempt + 1);
            } 
            
            getLogger().error("Ошибка отправки сообщения", error.getError());
            
            try {
                Integer interval = getSendTimeInterval();
                //Интервал для повторной отправки сообщения в ЕСУ без фиксации ошибок
                getLogger().info("Ожидание повторной отправки без фиксации ошибки, {} мсек", interval);
                Thread.sleep(interval);
            } catch (Exception e) {
                getLogger().error("Ошибка ожидания повторной отправки без фиксации ошибки", e);
                throw new RuntimeException(e);
            }
        }
        
        return Optional.ofNullable(error);
    }
    
    public static class ErrorObject {
        
        private final String message;
        private final Throwable error;
        private final int attempts; //количество попыток отправить сообщение
        
        public ErrorObject(String message, Throwable error, int attempts) {
            this.message = message;
            this.error = error;
            this.attempts = attempts;
        }

        public String getMessage() {
            return message;
        }

        public Throwable getError() {
            return error;
        }

        public int getAttempts() {
            return attempts;
        }
        
    }
    
}
