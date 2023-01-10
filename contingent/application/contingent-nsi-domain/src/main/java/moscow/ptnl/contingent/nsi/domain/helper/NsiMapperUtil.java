package moscow.ptnl.contingent.nsi.domain.helper;

import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import javax.persistence.Entity;
import moscow.ptnl.contingent.nsi.domain.NsiTablesEnum;
import moscow.ptnl.contingent.nsi.domain.annotation.MapToNsi;

/**
 *
 * @author m.kachalov
 */
public class NsiMapperUtil {
    
    private NsiMapperUtil(){}
    
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
    
    /**
     * 
     * @param <T>
     * @param value
     * @param fieldType
     * @return 
     */
    public static <T> T castSimpleType(Object value, Class<T> fieldType) {
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
    
    public static String valueToString(Object value) {
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
        if (value instanceof Boolean) {
            return (Boolean) value;
        } else if (value instanceof String) {
            String result = ((String) value).trim();
            if (result.isEmpty()) {
                return null;
            } else if ("1".equals(result)) {
                return true;
            } else {
                return Boolean.parseBoolean(result);
            }
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
    
    
    public static boolean isEntity(Class<?> type) {
        Entity e = type.getAnnotation(Entity.class);
        return e != null;
    }
    
}
