package moscow.ptnl.contingent.esu;

import moscow.ptnl.contingent.esu.service.EsuService;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

/**
 * Компонент для асинхронного выполнения задачи.
 * Делается отдельным компонентом, так как иначе асинхронность не сработает.  
 *
 * @author m.kachalov
 */
@Component
public class AsyncEsuExecutor {
    
    @Async 
    @Transactional(propagation = Propagation.REQUIRED, rollbackFor = Exception.class)
    public void publishToESU(final EsuService esuService, final Long recordId, final String publishTopic, final String message) {
        esuService.publishToESU(recordId, publishTopic, message);
    }   
    
}
