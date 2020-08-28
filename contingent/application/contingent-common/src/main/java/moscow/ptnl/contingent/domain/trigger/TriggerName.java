package moscow.ptnl.contingent.domain.trigger;

import java.util.Optional;

/**
 *
 * @author m.kachalov
 */
public enum TriggerName {
    
    /** Триггер очистки журнала ESU_INPUT (Т_К_1). */
    trigger_cleanup_esu_input("trigger_cleanup_esu_input"),
    
    /** Триггер очистки журнала ESU_OUTPUT (Т_К_2). */
    trigger_cleanup_esu_output("trigger_cleanup_esu_output");
    
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
