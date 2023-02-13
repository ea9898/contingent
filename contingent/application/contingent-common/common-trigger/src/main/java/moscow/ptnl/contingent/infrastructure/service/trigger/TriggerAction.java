package moscow.ptnl.contingent.infrastructure.service.trigger;

import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.Map;

import moscow.ptnl.contingent.domain.esu.EsuStatusType;
import moscow.ptnl.contingent.domain.trigger.TriggerName;
import moscow.ptnl.contingent.repository.esu.EsuInputCRUDRepository;
import moscow.ptnl.contingent.repository.esu.EsuOutputCRUDRepository;
import moscow.ptnl.contingent.infrastructure.service.setting.SettingService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
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
public class TriggerAction {
    
    private static final Logger LOG = LoggerFactory.getLogger(TriggerAction.class);
    
    private static final int MAX_DELETED_RECORDS = 1000;
    
    @Autowired
    private SettingService settingService;
    
    @Autowired
    private EsuInputCRUDRepository esuInputCRUDRepository;
    
    @Autowired
    private EsuOutputCRUDRepository esuOutputCRUDRepository;

    private final Map<TriggerName, Runnable> registeredActions;

    public TriggerAction() {
        registeredActions = new HashMap<>();
    }

    public Boolean action(TriggerName trigger){
        Boolean result = false;
        LOG.info("Попытка запуска триггера {}", trigger);
        try {
            switch (trigger) {
                case trigger_cleanup_esu_input:
                    result = triggerEsuInputCleanAction();
                break;
                case trigger_cleanup_esu_output:
                    result = triggerEsuOutputCleanAction();
                break;
                default:
                    result = runRegisteredAction(trigger);
                break;
            }
        } catch (Throwable e) {
            LOG.error("Ошибка выполнения триггера " + trigger, e);            
        }
        return result;
    }

    public void registerAction(TriggerName trigger, Runnable action, boolean replace) {
        if (!replace && registeredActions.containsKey(trigger)) {
            throw new IllegalStateException("Действие для триггера " + trigger + " уже зарегистрировано");
        }
        registeredActions.put(trigger, action);
    }

    //3. Система удаляет записи из ESU_INPUT
    private boolean triggerEsuInputCleanAction() {
        LOG.debug("Триггер удаления записей из ESU_INPUT");
        long startTime = System.currentTimeMillis();
        
        Boolean removeOnlySuccessRecords = settingService.getSettingProperty(SettingService.PAR_23, true);
        Long esuInputPeriodKeeping = settingService.getSettingProperty(SettingService.PAR_21, true);
        LocalDateTime beforeDate = LocalDateTime.now().minusDays(esuInputPeriodKeeping);
        
        Long deleted = 0L; 
        Page<Long> page = null;
        int pageNumber = 0;
        
        do {
            Pageable pageable = PageRequest.of(pageNumber, MAX_DELETED_RECORDS); //ограничиваем количество удаляемых за один проход записей
        
            if (Boolean.TRUE.equals(removeOnlySuccessRecords)) {
                //3.1 Если триггер должен удалять только успешно обработанные записи            
                page = esuInputCRUDRepository.findByReceivedTimeBeforeAndStatus(beforeDate, EsuStatusType.SUCCESS, pageable);            
            } else if (Boolean.FALSE.equals(removeOnlySuccessRecords)) {
                //3.2 Иначе триггер удаляет из ESU_INPUT записи
                page = esuInputCRUDRepository.findByReceivedTimeBefore(beforeDate, pageable);            
            }
            if (page != null && !page.isEmpty()) {
                deleted += esuInputCRUDRepository.deleteAllByIdInBatch(page.getContent());
            }
            
            pageNumber++;
        } while (page.hasNext());
        
        LOG.info("Триггер удалил {} записей из ESU_INPUT за {} мсек", deleted, (System.currentTimeMillis() - startTime));        
        return true;
    }
    
    //3. Система удаляет записи из ESU_OUTPUT
    private boolean triggerEsuOutputCleanAction() {
        LOG.debug("Триггер удаления записей из ESU_OUTPUT");
        Boolean removeOnlySuccessRecords = settingService.getSettingProperty(SettingService.PAR_24, true);
        Long esuOutputPeriodKeeping = settingService.getSettingProperty(SettingService.PAR_22, true);
        LocalDateTime beforeDate = LocalDateTime.now().minusDays(esuOutputPeriodKeeping);
        Long deleted = 0L;
        if (Boolean.TRUE.equals(removeOnlySuccessRecords)) {
            //3.1 Если триггер должен удалять только успешно обработанные записи 
            deleted = esuOutputCRUDRepository.deleteBySentTimeBeforeAndStatus(beforeDate, EsuStatusType.SUCCESS);
        } else if (Boolean.FALSE.equals(removeOnlySuccessRecords)) {
            //3.2
            deleted = esuOutputCRUDRepository.deleteBySentTimeBefore(beforeDate);
        }
        LOG.info("Триггер удалил {} записей из ESU_OUTPUT", deleted);
        return true;
    }

    private boolean runRegisteredAction(TriggerName triggerName) {
        Runnable action = registeredActions.get(triggerName);

        if (action == null) {
            LOG.error("Для триггера {} нет зарегистрированного действия", triggerName);
            return false;
        }
        action.run();

        return true;
    }
}
