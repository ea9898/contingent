package moscow.ptnl.contingent.domain.nsi.annotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import moscow.ptnl.contingent.domain.nsi.NsiTablesEnum;

/**
 * Аннотация которой помечается поле сущности мапируемое из НСИ.
 * Эту же аннотацию можно использовать для клсса сущности (чтобы задать имя
 * таблицы НСИ по умолчанию, которая будет использоваться для маппировки полей
 * сущности).
 * 
 * @author mkachalov
 */
@Target({ElementType.FIELD, ElementType.TYPE})
@Retention(RetentionPolicy.RUNTIME)
public @interface MapToNsi {
    
    /** 
     * Имя свойства из НСИ которое мапируется на поле.
     * Регистр символов игнорируется.  
     * По умолчанию для имени свойства берется имя поля сущности.
     * 
     * @return 
     */
    String value() default "";
    
    /**
     * Таблица из которой мапируются данные.
     * По умолчанию тащим это значение из аннотации которой помечена сущность.
     * 
     * @return 
     */
    NsiTablesEnum table() default NsiTablesEnum.UNKNOWN;
    
    
}
