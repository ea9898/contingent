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
           ) {
            return;
        }
        //2. Система записывает статус работы триггера, обновляя запись в таблице TRG_STATUS
        Future<Boolean> taskSetStatus = asyncRunService.run(() -> {
            //Нужно завершить транзакцию, чтобы сразу сохранить в БД. Использование saveAndFlush не работает
            //Также выполняем SELECT .. FOR UPDATE чтобы избежать параллельного запуска
            TriggerStatus status = triggerStatusRepository.findWithLock(trigger).orElse(new TriggerStatus(trigger));

            if (Boolean.TRUE.equals(status.getRun())
                    || !isAfterLastStartPlusInterval(status)) {
                return false;
            }
            status.setLastStartDate(LocalDateTime.now());
            status.setLastEndDate(null);
            status.setRun(Boolean.TRUE);
            triggerStatusRepository.save(status);
            return true;
        });
        Boolean result = Boolean.FALSE;
        long executionTimeLimit = getTriggerMaxExecutionTime(trigger);
        Future<Boolean> taskAction = null;

        try {
            if (!taskSetStatus.get(30, TimeUnit.SECONDS)) {
                //Триггер уже запущен
                return;
            }
            taskSetStatus = null;
            //3. Выполняем триггер (в отдельной транзакции с прерыванием по таймауту)
            taskAction = asyncRunService.run(() -> triggerAction.action(trigger));
            result = taskAction.get(executionTimeLimit, TimeUnit.MINUTES);
        } catch (TimeoutException e) {
            LOG.error("Выполнение триггера " + trigger + " прервано по таймауту", e);
            //Завершаем выполнение потока триггера
            if (taskSetStatus != null) {
                taskSetStatus.cancel(true);
            }
            if (taskAction != null) {
                taskAction.cancel(true);
            }
        } catch (InterruptedException e) {
            LOG.error("Выполнение триггера " + trigger + " прервано", e);
        } catch (ExecutionException e) {
            LOG.error("Выполнение триггера " + trigger + " завершилось ошибкой", e.getCause());
        }
        TriggerStatus status = triggerStatusRepository.findById(trigger).get();
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
            case trigger_cleanup_esu_input:
                return settingService.getSettingProperty(SettingService.PAR_34, true);            
            case trigger_cleanup_esu_output:
                return settingService.getSettingProperty(SettingService.PAR_33, true);           
            case trigger_synch_areainfo_k1:
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
            case trigger_cleanup_esu_input:
                rawString = settingService.getSettingProperty(SettingService.PAR_25, true);
            break;
            case trigger_cleanup_esu_output:
                rawString = settingService.getSettingProperty(SettingService.PAR_27, true);
            break;
            case trigger_synch_areainfo_k1:
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
     * @param status
     * @return 
     */
    private boolean isAfterLastStartPlusInterval(TriggerStatus status) {
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
