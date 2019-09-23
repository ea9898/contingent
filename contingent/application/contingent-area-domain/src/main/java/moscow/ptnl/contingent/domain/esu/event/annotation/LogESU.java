package moscow.ptnl.contingent.domain.esu.event.annotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.lang.reflect.Method;

/**
 * Аннотация которой помечаются логируемые методы сервиса.
 * 
 * @author mkachalov
 */
@Target(ElementType.METHOD)
@Retention(RetentionPolicy.RUNTIME)
public @interface LogESU {
    
    /**
     * Тип логируемого события (например {@link moscow.ptnl.contingent.domain.esu.event.AreaInfoEvent}).
     * 
     * @return 
     */
    Class<?> type();
    
    String[] parameters() default {};
    
    boolean useResult() default false;

    String methodName() default "";
    
}
