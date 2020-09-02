package moscow.ptnl.contingent.infrastructure.service.trigger;

import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import moscow.ptnl.contingent.domain.trigger.TriggerName;
import moscow.ptnl.contingent.domain.trigger.TriggerStatus;
import moscow.ptnl.contingent.domain.trigger.TriggerHistoryItem;
import moscow.ptnl.contingent.infrastructure.service.TransactionRunService;
import moscow.ptnl.contingent.infrastructure.service.setting.SettingService;
import moscow.ptnl.contingent.repository.trigger.TriggerStatusCRUDRepository;
import moscow.ptnl.contingent.repository.trigger.TriggerHistoryItemCRUDRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

/**
 *
 * @author m.kachalov
 */
@Service
@Transactional(propagation = Propagation.REQUIRED)
public class TriggerService {
    
    private static final Logger LOG = LoggerFactory.getLogger(TriggerService.class);
    
    @Autowired
    private SettingService settingService;
    
    @Autowired
    private TriggerStatusCRUDRepository triggerStatusRepository;
    
    @Autowired
    private TriggerHistoryItemCRUDRepository triggerHistoryRepository;
    
    @Autowired
    private TriggerAction triggerAction;
    
    @Autowired
    private TransactionRunService asyncRunService;

    public void registerAction(TriggerName trigger, Runnable action) {
        triggerAction.registerAction(trigger, action, false);
    }

    public void startTrigger(TriggerName trigger) {
        Objects.requireNonNull(trigger);
        
        //1. Система проверяет, что триггер должен и может запуститься
        if (
                isDoNotRunStatus(trigger)
                || !isInTimeInterval(trigger)
                || !isAfterLastStartPlusInterval(trigger)
                || isTriggerRunned(trigger)
           ) {
            return;
        }
        
        //2. Система записывает статус работы триггера, обновляя запись в таблице TRG_STATUS
        TriggerStatus status = triggerStatusRepository.findById(trigger).orElse(new TriggerStatus(trigger));
        status.setLastStartDate(LocalDateTime.now());
        status.setLastEndDate(null);
        status.setRun(Boolean.TRUE);
        triggerStatusRepository.saveAndFlush(status);
        
        //3. Выполняем триггер (в отдельной транзакции с прерыванием по таймауту)
        long executionTimeLimit = getTriggerMaxExecutionTime(trigger);
        Future<Boolean> task = asyncRunService.run(() -> {
            return triggerAction.action(trigger);
        });
        Boolean result = Boolean.FALSE;
        try {
            result = task.get(executionTimeLimit, TimeUnit.MINUTES);
        } catch (TimeoutException e) {
            LOG.error("Выполнение триггера " + trigger + " прервано по таймауту", e);
            //Завершаем выполнение потока триггера
            task.cancel(true);
        } catch (InterruptedException e) {
            LOG.error("Выполнение триггера " + trigger + " прервано", e);
        } catch (ExecutionException e) {
            LOG.error("Выполнение триггера " + trigger + " завершилось ошибкой", e.getCause());
        }
        //Меняем статус триггера
        status.setRun(Boolean.FALSE);
        status.setLastEndDate(LocalDateTime.now());
        triggerStatusRepository.save(status);
        
        //Производим запись в историю
        TriggerHistoryItem historyItem = new TriggerHistoryItem();
        historyItem.setTrigger(trigger);
        historyItem.setStartTime(status.getLastStartDate());
        historyItem.setEndTime(status.getLastEndDate());
        
        //4. Если при удалении записей получена ошибка
        if (Boolean.FALSE.equals(result)) {
            historyItem.setResult(0L);
        } else if (Boolean.TRUE.equals(result)) {
            historyItem.setResult(1L);
        }
        
        triggerHistoryRepository.save(historyItem);
    }
    
    /**
     * Максимальное время выполнения триггера (в минутах).
     * 
     * @param trigger
     * @return если такой настройки для триггера нет, то 0
     */
    private long getTriggerMaxExecutionTime(TriggerName trigger) {
        switch (trigger) {
            case TRIGGER_CLEANUP_ESU_INPUT:
                return settingService.getSettingProperty(SettingService.PAR_34, true);            
            case TRIGGER_CLEANUP_ESU_OUTPUT:
                return settingService.getSettingProperty(SettingService.PAR_33, true);           
            case TRIGGER_SYNCH_AREAINFO_K_1:
                return settingService.getSettingProperty(SettingService.PAR_37, true);
        }
        return 0;
    }
    
    /**
     * Проверка нахождения триггера в списке запрещенных к запуску: 
     * PAR_29 (do_not_run_triggers).
     * 
     * @param trigger
     * @return 
     */
    private boolean isDoNotRunStatus(TriggerName trigger) {
        String rawString = settingService.getSettingProperty(SettingService.PAR_29, true);
        if (rawString != null) {
            return rawString.contains(trigger.getName());
        }
        return false;
    }
    
    /**
     * Проверка попал ли триггер в интервал (если на него нет настройки, то
     * считаем что попал).
     * 
     * @param trigger
     * @return 
     */
    private boolean isInTimeInterval(TriggerName trigger) {
        String rawString = null;
        switch (trigger) {
            case TRIGGER_CLEANUP_ESU_INPUT:
                rawString = settingService.getSettingProperty(SettingService.PAR_25, true);
            break;
            case TRIGGER_CLEANUP_ESU_OUTPUT:
                rawString = settingService.getSettingProperty(SettingService.PAR_27, true);
            break;
            case TRIGGER_SYNCH_AREAINFO_K_1:
                rawString = settingService.getSettingProperty(SettingService.PAR_35, true);
            break;
        }
        if (rawString != null && rawString.trim().length() > 0) {
            //HH:mm-HH:mm
            return SettingService.timeInInterval(rawString, LocalTime.now());
        }
        return true;
    }
    
    /**
     * Проверка, что с прошлого запуска триггера прошло достаточно времени.
     * 
     * @param trigger
     * @return 
     */
    private boolean isAfterLastStartPlusInterval(TriggerName trigger) {
        Optional<TriggerStatus> status = triggerStatusRepository.findById(trigger);
        if (status.isPresent()) {
            Long interval = null;
            switch (trigger) {
                case TRIGGER_CLEANUP_ESU_INPUT:
                    interval = settingService.getSettingProperty(SettingService.PAR_26, true);
                break;
                case TRIGGER_CLEANUP_ESU_OUTPUT:
                    interval = settingService.getSettingProperty(SettingService.PAR_28, true);
                break;
                case TRIGGER_SYNCH_AREAINFO_K_1:
                    interval = settingService.getSettingProperty(SettingService.PAR_36, true);
                break;
            }
            if (interval != null && interval > 0) {
                return status.get().getLastStartDate().plusMinutes(interval).isBefore(LocalDateTime.now());
            }
        }        
        return true;
    }
    
    /**
     * Проверяет не запущен ли уже (еще) триггер.
     * 
     * @param trigger
     * @return 
     */
    private boolean isTriggerRunned(TriggerName trigger) {
        Optional<TriggerStatus> status = triggerStatusRepository.findById(trigger);
        if (status.isPresent()) {
            return status.get().getRun();
        }
        return false;
    }
    
}
