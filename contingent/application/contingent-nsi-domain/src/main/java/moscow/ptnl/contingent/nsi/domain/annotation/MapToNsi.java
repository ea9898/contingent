package moscow.ptnl.contingent.nsi.domain.annotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import moscow.ptnl.contingent.nsi.domain.NsiTablesEnum;

/**
 * Аннотация которой помечается поле сущности мапируемое из НСИ.
 * Эту же аннотацию можно использовать для клсса сущности (чтобы задать имя
 * таблицы НСИ по умолчанию, которая будет использоваться для маппировки полей
 * сущности, остальные свойства при этом игнорируются).
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
    
    /**
     * Имя поля связанной сущности, по которому она будет искаться, 
     * если маппинг производится на связанный объект. 
     * Поле не обязательно ключевое, но поиск по этому полю должен давать 
     * уникальный результат.
     * Имя поля регистрозависимое. 
     * Пример: "id", "code". 
     * @return 
     */
    String findEntityByField() default "";

    /**
     * Промежуточный объект из которого надо брать данные
     * Напрмер AreaTypeMedicalPositions.positionCode, должен браться из PositionNom.positionCode
     */
    Class crossObject() default Object.class;

    /**
     * Поле промежуточного объекта по которому ищем объект
     * Напрмер в AreaTypeMedicalPositions PositionNom.positionCode ищется по globalId.
     */
    String crossField() default "";

}
