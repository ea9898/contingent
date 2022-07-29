package moscow.ptnl.contingent.domain.history.meta;

/**
 * На какой вид события должно логироваться аннотированное (@see LogIt) поле.
 * 
 * @author m.kachalov
 */
public enum LogTrigger {
    
    /** Всегда (если не пустое). */
    ALWAYS,
    /** Если значение поля изменилось. */
    ONCHANGE;
    
}
