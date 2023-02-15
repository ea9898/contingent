package moscow.ptnl.contingent.nsi.domain.helper;

import java.lang.reflect.Field;
import jakarta.persistence.EntityManager;
import jakarta.persistence.Id;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.Query;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Root;
import moscow.ptnl.contingent.PersistenceConstraint;
import moscow.ptnl.contingent.nsi.domain.annotation.MapToNsi;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;
import org.springframework.util.ReflectionUtils;

/**
 *
 * @author mkachalov
 */
@Component
public class MapToNsiHelper {
    
    private final static Logger LOG = LoggerFactory.getLogger(MapToNsiHelper.class);
    
    @PersistenceContext(unitName = PersistenceConstraint.PU_CONTINGENT_NAME)
    private EntityManager entityManager;
    
    public void setFieldValue(Field field, Object target, Object value, MapToNsi mapToNsi) {   
        try {
            if (value == null || (value instanceof String && String.valueOf(value).length() == 0)) {
                return;
            }

            field.setAccessible(true);
            if (mapToNsi == null || mapToNsi.crossObject().equals(Object.class) || mapToNsi.crossField().isEmpty()) {
                field.set(target, cast(value, field.getType(), (mapToNsi != null) ? mapToNsi.findEntityByField() : ""));
            } else {
                field.set(target, crossCast(value, field.getType(), mapToNsi.findEntityByField(), mapToNsi.crossObject(), mapToNsi.crossField()));
            }
        } catch (Exception e) {
            String msg = "ошибка заполнения поля [" + field.getName() +"] для типа [" + target.getClass() + "] значением [" + value + "], " + e.getMessage();
            LOG.error(msg);
            throw new IllegalStateException(msg);
        }
    }

    // Михаилу на проверку решения:)
    private <T> T crossCast(Object value, Class<T> fieldType, String fieldName, Class crossType, String crossFieldName) throws Exception {
        Field crossField = ReflectionUtils.findField(crossType, fieldName);
        crossField.setAccessible(true);
        return (T) crossField.get(findEntityByField(crossType, crossFieldName, value));
    }

    private <T> T cast(Object value, Class<T> fieldType, String fieldName) throws Exception {
        //если поле это связь с другой сущностью
        if (NsiMapperUtil.isEntity(fieldType) && fieldName != null && !fieldName.isEmpty()) {
            //получаем имя поля в связанной сущности на которое будет мапится поле
            String entityFieldName = (fieldName.contains(".")) ? fieldName.substring(fieldName.lastIndexOf(".")) : fieldName;
            return valueToEntity(value, fieldType, entityFieldName);
        }
        
        //если поле простого типа
        return NsiMapperUtil.castSimpleType(value, fieldType);
    }
    
    private <T> T valueToEntity(Object fieldValue, Class<T> entityType, String mapTo) throws InstantiationException, IllegalAccessException, NoSuchFieldException {       
        Field field = entityType.getDeclaredField(mapTo);
        
        //если сущность ищем по ключевому полю
        if (field.getAnnotation(Id.class) != null) {
            T entityObject = entityType.newInstance();
            setFieldValue(field, entityObject, fieldValue, null);
            return entityObject;
        } else {
            return findEntityByField(entityType, mapTo, fieldValue);
        }
    }
    
    private <T> T findEntityByField(Class<T> entityType, String fieldName, Object fieldValue) {
        try {
            CriteriaBuilder builder = entityManager.getCriteriaBuilder();
            CriteriaQuery<T> cQuery = builder.createQuery(entityType);
            Root<T> entity = cQuery.from(entityType);
            Query query = entityManager.createQuery(cQuery.where(builder.equal(entity.get(fieldName), fieldValue)));
            return (T) query.getSingleResult();
        } catch (Exception e) {
            LOG.error("ошибка мапинга типа: {} по полю: {} со значением: {}", entityType.getName(), fieldName, fieldValue);
            throw e;
        }
    }
    
}
