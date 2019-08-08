package moscow.ptnl.contingent.repository.esu;

import moscow.ptnl.contingent.domain.esu.EsuStatusType;
import org.springframework.data.repository.NoRepositoryBean;

import java.time.LocalDateTime;
import java.util.List;
import moscow.ptnl.contingent.domain.esu.EsuOutput;

@NoRepositoryBean
public interface EsuOutputRepository {
        
    /**
     * Выбирает записи которые должны быть повторно отправлены в ЕСУ 
     * (SUCCESS = 0 и SENT_TIME меньше заданой даты) и проставляет им
     * статус "в процессе отправки" (SUCCESS = 2).
     * 
     * @param olderThan
     * @return 
     */
    List<EsuOutput> findEsuOutputsToResend(LocalDateTime olderThan);
    
    /**
     * Меняем статус группе записей с указанными id.
     * 
     * @param ids
     * @param fromStatus первоначальное значение статуса (учитывается в услрвии)
     * @param toStatus новое значение статуса
     */
    void updateStatus(List<Long> ids, EsuStatusType fromStatus, EsuStatusType toStatus, String hostName);
    
    /**
     * Меняем статус записи с указанным id.
     * 
     * @param id
     * @param fromStatus первоначальное значение статуса (учитывается в услрвии)
     * @param toStatus новое значение статуса
     */
    void updateStatus(Long id, EsuStatusType fromStatus, EsuStatusType toStatus, String hostName);
    
    /**
     * Меняем сообщение записи с указанным id.
     * 
     * @param id
     * @param message 
     */
    void updateMessage(Long id, String message);
}
