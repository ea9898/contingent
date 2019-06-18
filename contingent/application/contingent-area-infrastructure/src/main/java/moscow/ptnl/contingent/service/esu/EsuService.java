package moscow.ptnl.contingent.service.esu;


import java.time.LocalDateTime;

/**
 * Сервис публикации данных в ЕСУ и сохранение статусов в БД.
 * 
 * @author m.kachalov
 */
public interface EsuService {

    /**
     * Сохраняет информацию о событии в БД и пытается отправить ее в ЕСУ.
     * В случае успеха отправки в ЕСУ, ответ сохраняется в БД.
     *
     * @param topicName
     * @param event
     * @return true при успешной публикации в ЕСУ
     */
    boolean saveAndPublishToESU(String topicName, Object event);

    void periodicalPublishUnsuccessMessagesToESU(LocalDateTime olderThan);
    
    /**
     * Метод для асинхронного вызова в компоненте {@see AsyncEsuExecutor}.
     * 
     * @param recordId
     * @param publishTopic
     * @param message 
     */
    void publishToESU(final Long recordId, final String publishTopic, final String message);
}
