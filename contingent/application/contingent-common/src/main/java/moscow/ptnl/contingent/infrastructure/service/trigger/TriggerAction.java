package moscow.ptnl.contingent.infrastructure.service.trigger;

import java.time.LocalDateTime;
import moscow.ptnl.contingent.domain.esu.EsuStatusType;
import moscow.ptnl.contingent.domain.trigger.TriggerName;
import moscow.ptnl.contingent.infrastructure.service.setting.SettingService;
import moscow.ptnl.contingent.repository.esu.EsuInputCRUDRepository;
import moscow.ptnl.contingent.repository.esu.EsuOutputCRUDRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 *
 * @author m.kachalov
 */
@Component
public class TriggerAction {
    
    private static final Logger LOG = LoggerFactory.getLogger(TriggerAction.class);
    
    @Autowired
    private SettingService settingService;
    
    @Autowired
    private EsuInputCRUDRepository esuInputCRUDRepository;
    
    @Autowired
    private EsuOutputCRUDRepository esuOutputCRUDRepository;
    
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
            } 
        } catch (Throwable e) {
            LOG.error("Ошибка выполнения триггера " + trigger, e);            
        }
        return result;
    }
    
    //3. Система удаляет записи из ESU_INPUT
    private boolean triggerEsuInputCleanAction() {
        LOG.debug("Триггер удаления записей из ESU_INPUT");
        Boolean removeOnlySuccessRecords = settingService.getSettingProperty(SettingService.PAR_23, true);
        Long esuInputPeriodKeeping = settingService.getSettingProperty(SettingService.PAR_21, true);
        LocalDateTime beforeDate = LocalDateTime.now().minusDays(esuInputPeriodKeeping);
        Long deleted = 0L;
        if (Boolean.TRUE.equals(removeOnlySuccessRecords)) {
            //3.1 Если триггер должен удалять только успешно обработанные записи            
            deleted = esuInputCRUDRepository.deleteByReceivedTimeBeforeAndStatus(beforeDate, EsuStatusType.SUCCESS);
        } else if (Boolean.FALSE.equals(removeOnlySuccessRecords)) {
            //3.2 Иначе триггер удаляет из ESU_INPUT записи
            deleted = esuInputCRUDRepository.deleteByReceivedTimeBefore(beforeDate);
        }
        LOG.info("Триггер удалил {} записей из ESU_INPUT", deleted);        
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
    
}
