/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package moscow.ptnl.contingent.domain.nsi.annotation;

import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.math.BigDecimal;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import moscow.ptnl.contingent.domain.nsi.NsiTablesEnum;

/**
 *
 * @author mkachalov
 */
public class MapToNsiHelper {
    
    private MapToNsiHelper() {}
    
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
    
    public static void setFieldValue(Field field, Object target, Object value, MapToNsi mapToNsi) {   
        try {
            field.setAccessible(true);
            field.set(target, cast(value, field.getType()));
        } catch (Exception e) {
            throw new IllegalStateException(e);
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
    
    private static <T> T cast(Object value, Class<T> toType) {
        if (value == null) {
            return null;
        }
        
        switch (toType.getSimpleName()) {
            case "String":
                return toType.cast(valueToString(value));
            case "Long":
                return toType.cast(valueToLong(value));
            case "Boolean":
                return toType.cast(valueToBoolean(value));
            default:
                throw new IllegalArgumentException("не поддерживаемый тип поля для сущности: [" + toType.getSimpleName() +"]");                
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
    
}
