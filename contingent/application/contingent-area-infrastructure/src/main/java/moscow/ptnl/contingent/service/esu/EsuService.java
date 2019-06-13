package moscow.ptnl.contingent.service.esu;


import java.time.LocalDateTime;
import moscow.ptnl.contingent.domain.esu.EsuOutput;


public interface EsuService {

    /**
     * Сохраняет информацию о событии в БД и пытается отправить ее в ЕСУ.
     * В случае успеха отправки в ЕСУ, ответ сохраняется в БД.
     *
     * @param topicName
     * @param event
     * @return true при успешной публикации в ЕСУ
     */
    boolean saveAndPublishToESU(String topicName, EsuOutput event);

    void periodicalPublishUnsuccessMessagesToESU(LocalDateTime olderThan);
}
