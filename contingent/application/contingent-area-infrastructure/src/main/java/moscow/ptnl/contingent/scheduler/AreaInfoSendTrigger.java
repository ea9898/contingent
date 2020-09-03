package moscow.ptnl.contingent.scheduler;

import moscow.ptnl.contingent.domain.area.EsuHelperService;
import moscow.ptnl.contingent.domain.area.entity.Area;
import moscow.ptnl.contingent.domain.area.repository.AreaRepository;
import moscow.ptnl.contingent.domain.trigger.TriggerName;
import moscow.ptnl.contingent.infrastructure.service.setting.SettingService;
import moscow.ptnl.contingent.infrastructure.service.trigger.TriggerService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.PropertySource;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import java.util.List;

/**
 * @author sorlov
 * Т_К2_1
 * Триггер формирования топика AreaInfo для участков с МР с окончившимся сроком действия
 */
@Component
@PropertySource("classpath:application-esu.properties")
public class AreaInfoSendTrigger implements Runnable {

    @Autowired
    private EsuHelperService esuHelperService;

    @Autowired
    private SettingService settingService;

    @Autowired
    private TriggerService triggerService;

    @Autowired
    private AreaRepository areaRepository;

    @PostConstruct
    public void setup() {
        //Регистрируем действие для триггера
        triggerService.registerAction(TriggerName.trigger_synch_areainfo_k1, this);
    }

    /**
     * Периодический запуск триггера
     */
    @Scheduled(cron = "${area-info.sync.k1.cron.rule}")
    public void triggerScheduler() {
        triggerService.startTrigger(TriggerName.trigger_synch_areainfo_k1);
    }

    @Override
    public void run() {
        //3. Система выполняет поиск уникальных ИД участков (SELECT DISTINCT) в таблице AREAS
        // с использованием сведений из таблицы AREA_MEDICAL_EMPLOYEES
        List<Area> areas = areaRepository.findAreasForSyncToK1(settingService.getSettingProperty(SettingService.PAR_38));
        //4. Для каждого участка, найденного на предыдущем шаге, Система формирует топик AreaInfo
        for (Area area : areas) {
            esuHelperService.sendAreaInfoEvent(area, TriggerName.trigger_synch_areainfo_k1.getName());
        }
    }
}
