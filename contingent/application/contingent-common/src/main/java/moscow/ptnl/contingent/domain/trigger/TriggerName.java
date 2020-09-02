package moscow.ptnl.contingent.domain.trigger;

import java.util.Optional;

/**
 *
 * @author m.kachalov
 */
public enum TriggerName {
    
    /** Триггер очистки журнала ESU_INPUT (Т_К_1). */
    TRIGGER_CLEANUP_ESU_INPUT("trigger_cleanup_esu_input"),
    
    /** Триггер очистки журнала ESU_OUTPUT (Т_К_2). */
    TRIGGER_CLEANUP_ESU_OUTPUT("trigger_cleanup_esu_output"),

    /** Триггер формирования топика AreaInfo для участков с МР с окончившимся сроком действия (Т_К2_1). */
    TRIGGER_SYNCH_AREAINFO_K_1("trigger_synch_areainfo_k1");

    private final String name;
    
    TriggerName(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }
    
    public static Optional<TriggerName> getByName(String name) {
        for (TriggerName t : values()) {
            if (t.getName().equals(name)) {
                return Optional.of(t);
            }
        }
        return Optional.empty();
    }
    
}
