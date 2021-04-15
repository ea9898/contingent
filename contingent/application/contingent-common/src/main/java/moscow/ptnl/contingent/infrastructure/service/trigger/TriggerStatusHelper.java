package moscow.ptnl.contingent.infrastructure.service.trigger;

import java.time.LocalDateTime;
import moscow.ptnl.contingent.domain.trigger.TriggerName;
import moscow.ptnl.contingent.domain.trigger.TriggerStatus;
import moscow.ptnl.contingent.infrastructure.service.setting.SettingService;
import moscow.ptnl.contingent.repository.trigger.TriggerStatusCRUDRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

/**
 *
 * @author m.kachalov
 */
@Transactional(
        propagation = Propagation.REQUIRED,
        rollbackFor = Throwable.class
)
@Component
public class TriggerStatusHelper {
    
    private static final Logger LOG = LoggerFactory.getLogger(TriggerStatusHelper.class);
    
    @Autowired
    private SettingService settingService;
    
    @Autowired
    private TriggerStatusCRUDRepository triggerStatusRepository;
    
    @Transactional(
            propagation = Propagation.REQUIRES_NEW,
            noRollbackFor = Throwable.class
    )
    public boolean setRunOnStatus(final TriggerName trigger) {
        try {
            //Нужно завершить транзакцию, чтобы сразу сохранить в БД. Использование saveAndFlush не работает
            //Также выполняем SELECT .. FOR UPDATE чтобы избежать параллельного запуска
            TriggerStatus status = triggerStatusRepository
                    .findWithLock(trigger)
                    .orElse(new TriggerStatus(trigger));

             if (Boolean.TRUE.equals(status.getRun())
                    || !isAfterLastStartPlusInterval(status)) {
                return false;
            }

            status.setLastStartDate(LocalDateTime.now());
            status.setLastEndDate(null);
            status.setRun(Boolean.TRUE);
            triggerStatusRepository.save(status);
            
            return true;
        } catch (Throwable e) {
            LOG.error("Ошибка выполнения триггера " + trigger, e);
            e.printStackTrace();
        }
        return false;
    }
    
    public TriggerStatus setRunOffStatus(final TriggerName trigger) {
        TriggerStatus status = triggerStatusRepository.findById(trigger).get();
        
        status.setRun(Boolean.FALSE);
        status.setLastEndDate(LocalDateTime.now());
        triggerStatusRepository.save(status);
        return status;
    }
    
    /**
     * Проверка, что с прошлого запуска триггера прошло достаточно времени.
     * 
     * @param status
     * @return 
     */
    private boolean isAfterLastStartPlusInterval(final TriggerStatus status) {
        if (status.getLastStartDate() != null) {
            Long interval = null;
            switch (status.getTrigger()) {
                case trigger_cleanup_esu_input:
                    interval = settingService.getSettingProperty(SettingService.PAR_26, true);
                break;
                case trigger_cleanup_esu_output:
                    interval = settingService.getSettingProperty(SettingService.PAR_28, true);
                break;
                case trigger_synch_areainfo_k1:
                    interval = settingService.getSettingProperty(SettingService.PAR_36, true);
                break;
            }
            if (interval != null && interval > 0) {
                return status.getLastStartDate().plusMinutes(interval).isBefore(LocalDateTime.now());
            }
        }        
        return true;
    }
    
}
