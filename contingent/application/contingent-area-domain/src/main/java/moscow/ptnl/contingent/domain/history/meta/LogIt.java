package moscow.ptnl.contingent.domain.history.meta;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import moscow.ptnl.contingent.domain.history.converter.DefaultConverter;

/**
 * Аннотация которой помечается поле участвующее в логировании изменений.
 * @author m.kachalov
 */
@Target(value = ElementType.FIELD)
@Retention(value = RetentionPolicy.RUNTIME)
@Documented
public @interface LogIt {
    
    /**
     * Класс осуществляющий преобразование поля в форму пригодную для логирования.
     * 
     * @return 
     */
    Class<? extends FieldConverter> converter() default DefaultConverter.class;
    
    /**
     * В каком случае производить запись в лог.
     * 
     * @return 
     */
    LogTrigger trigger() default LogTrigger.ONCHANGE;
    
}
