package moscow.ptnl.contingent.domain.history.meta;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import moscow.ptnl.contingent.domain.history.ServiceName;

/**
 * Аннотация для сущности поля которой могут журналироваться (изменения полей 
 * пишутся в журнал истории изменений).
 * Логируемые поля должны быть аннотированы как {@see LogIt}.
 * 
 * @author m.kachalov
 */
@Target(value = ElementType.TYPE)
@Retention(value = RetentionPolicy.RUNTIME)
@Documented
public @interface Journalable {
    
    ServiceName value();
    
}
