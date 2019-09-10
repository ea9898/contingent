package moscow.ptnl.contingent.nsi.domain.annotation;

import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.math.BigDecimal;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import javax.persistence.Entity;
import javax.persistence.EntityManager;
import javax.persistence.Id;
import javax.persistence.PersistenceContext;
import javax.persistence.Query;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Root;
import moscow.ptnl.contingent.PersistenceConstraint;
import moscow.ptnl.contingent.nsi.domain.NsiTablesEnum;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

/**
 *
 * @author mkachalov
 */
@Component
public class MapToNsiHelper {
    
    private final static Logger LOG = LoggerFactory.getLogger(MapToNsiHelper.class);
    
    @PersistenceContext(unitName = PersistenceConstraint.PU_CONTINGENT_NAME)
    private EntityManager entityManager;        
    
    public static Optional<NsiTablesEnum> getNsiTableFromClass(Class entity) {
        Annotation ann = entity.getAnnotation(MapToNsi.class);
        if (ann != null) {
            MapToNsi mapToNsi = (MapToNsi) ann;
            NsiTablesEnum nsiTablesEnum = mapToNsi.table();
            if (!NsiTablesEnum.UNKNOWN.equals(nsiTablesEnum)) {
                return Optional.of(nsiTablesEnum);
            }
        }
        return Optional.empty();
    }
    
    public static Map<Field, MapToNsi> getNsiAnnotatedFields(Class entity) {
        Map<Field, MapToNsi> annotatedFields = new HashMap<>();
        
        Field[] fields = entity.getDeclaredFields();
        for (int i = 0; i < fields.length; i++) {
            Field field = fields[i];
            Annotation ann = field.getAnnotation(MapToNsi.class);
            if (ann != null) {
                annotatedFields.put(field, (MapToNsi) ann);
            }
        }
        
        return annotatedFields;
    }
    
    public static String getNsiFieldName(Field field, MapToNsi mapToNsi) {
        String nsiFieldName = mapToNsi.value();
        if (nsiFieldName.isEmpty()) {
            nsiFieldName = field.getName();
        }
        return nsiFieldName;
    }
    
    public void setFieldValue(Field field, Object target, Object value, MapToNsi mapToNsi) {   
        try {
            field.setAccessible(true);
            field.set(target, cast(value, field.getType(), (mapToNsi != null) ? mapToNsi.findEntityByField() : ""));
        } catch (Exception e) {
            String msg = "ошибка заполнения поля [" + field.getName() +"] для типа [" + target.getClass() + "] значением [" + value + "], " + e.getMessage();
            LOG.error(msg);
            throw new IllegalStateException(msg);
        }
    }
    
    private String getSetterMethodName(Field field) {
        String fieldName = field.getName();
        String methodName = "set" + fieldName.substring(0, 1).toUpperCase();
        if (fieldName.length() > 1) {
            methodName += fieldName.substring(1);
        }
        return methodName;
    }
    
    private <T> T cast(Object value, Class<T> fieldType, String fieldName) throws Exception {
        if (value == null) {
            return null;
        }
        
        //если поле это связь с другой сущностью
        if (isEntity(fieldType) && fieldName != null && !fieldName.isEmpty()) {
            //получаем имя поля в связанной сущности на которое будет мапится поле
            String entityFieldName = (fieldName.contains(".")) ? fieldName.substring(fieldName.lastIndexOf(".")) : fieldName;
            return valueToEntity(value, fieldType, entityFieldName);
        }
        
        //если поле простого типа
        switch (fieldType.getSimpleName()) {
            case "String":
                return fieldType.cast(valueToString(value));
            case "Long":
                return fieldType.cast(valueToLong(value));
            case "Integer":
                return fieldType.cast(valueToInteger(value));
            case "Boolean":
                return fieldType.cast(valueToBoolean(value));
            case "LocalDate":
                return fieldType.cast(valueToLocalDate(value));
            default:
                throw new IllegalArgumentException("не поддерживаемый тип поля для сущности: [" + fieldType.getSimpleName() +"]");                
        }
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
    
    private static String valueToString(Object value) {
        if (value == null) {
            return null;
        }
        String result = value.toString();
        return (result.isEmpty()) ? null : result;
    }
    
    private static Long valueToLong(Object value) {
        if (value == null) {
            return null;
        }
        if (value instanceof BigDecimal) {
            return ((BigDecimal) value).longValue();
        } else if (value instanceof String) {
            return Long.parseLong((String) value);
        }
        throw new IllegalArgumentException("не поддерживаемый тип: [" + value.getClass().getName() + "]");
    }
    
    private static Integer valueToInteger(Object value) {
        if (value == null) {
            return null;
        }
        if (value instanceof BigDecimal) {
            return ((BigDecimal) value).intValue();
        } else if (value instanceof String) {
            return Integer.parseInt((String) value);
        }
        throw new IllegalArgumentException("не поддерживаемый тип: [" + value.getClass().getName() + "]");
    }
    
    private static Boolean valueToBoolean(Object value) {
        if (value == null) {
            return null;
        }
        if (value instanceof String) {
            String result = (String) value;
            return (result.isEmpty()) ? null : "1".equals(result);
        }
        throw new IllegalArgumentException("не поддерживаемый тип: [" + value.getClass().getName() + "]");
    }

    private static LocalDate valueToLocalDate(Object value) {
        if (value == null) {
            return null;
        }

        if (value instanceof String) {
            String result = (String) value;
            LocalDate localDate = LocalDateTime.parse(result, DateTimeFormatter.ofPattern("dd.MM.yyyy HH:mm:ss")).toLocalDate();
            return localDate;
        }
        throw new IllegalArgumentException("не поддерживаемый тип: [" + value.getClass().getName() + "]");
    }

    private static boolean isEntity(Class<?> type) {
        Entity e = type.getAnnotation(Entity.class);
        return e != null;
    }
    
}
