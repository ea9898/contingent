package moscow.ptnl.contingent.domain.history;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.text.SimpleDateFormat;
import java.time.format.DateTimeFormatter;
import java.util.Collection;
import java.util.HashSet;
import java.util.StringJoiner;
import javax.persistence.Entity;
import moscow.ptnl.util.CollectionsUtil;
import moscow.ptnl.util.Strings;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Вспомогательный класс для построения конвертеров сущностей и их полей в
 * строковое представление.
 * 
 * @author m.kachalov
 */
public final class EntityConverterHelper {
    
    private static final Logger LOG = LoggerFactory.getLogger(EntityConverterHelper.class);
    
    private final static SimpleDateFormat DATE_FORMAT = new SimpleDateFormat("dd.MM.yyyy HH:mm:ss");
    private final static DateTimeFormatter LOCAL_DATE_TIME_FORMAT = DateTimeFormatter.ofPattern("dd.MM.yyyy HH:mm:ss");
    private final static DateTimeFormatter LOCAL_DATE_FORMAT = DateTimeFormatter.ofPattern("yyyy-MM-dd");
    public static final String COLLECTION_DELIMITER = ",";
    public static final String COMPOSITE_KEY_DELIMITER = ",";
    
    private EntityConverterHelper(){}
    
    /**
     * Получаем строковое представление ключа сущности.
     * У сущности должен быть метод "getKey()" или "getId()".
     * 
     * @param entity
     * @return 
     */
    public static String getEntityId(Object entity) {
        if (entity == null) {
            throw new IllegalArgumentException("аргумент не может быть null");
        }
        String result = null;
        try {
            for(Method method : entity.getClass().getDeclaredMethods()) {
                String methodName = method.getName();
                if ("getKey".equals(methodName) || "getId".equals(methodName)) { //порядок проверки имеет значение
                    Object value = method.invoke(entity);
                    if (value == null) {
                        throw new Exception("ключ не инициализирован");
                    }
                    if ("getId".equals(methodName)) { //простой ключ с именем "id"
                        result = value.toString();
                    } else if ("getKey".equals(methodName)) { //получаем текстовое представление композитного ключа
                        result = getKeyAsString(value);
                    }
                    break;
                }
            }
        } catch (Exception e) {
            LOG.error("ошибка получения ключа сущности", e);
            throw new RuntimeException(e);
        }
        return result;
    }
    
    /**
     * Текстовое представление композитного ключа.
     * {имяПоля:значение,имяПоля:значение}
     * 
     * @param key
     * @return 
     */
    public static String getKeyAsString(Object key) {
        StringBuilder sb = new StringBuilder();
        sb.append("{");
        for (Field f : key.getClass().getDeclaredFields()) {
            f.setAccessible(true);
            String fieldName = f.getName();
            if (sb.length() > 1) {
                sb.append(COMPOSITE_KEY_DELIMITER);
            }
            sb.append(fieldName).append(":");
            try {
                sb.append(toString(key));
            } catch (Exception e) {
                LOG.error("ошибка получения текстового представления поля: " + fieldName);
                throw e;
            }
        }
        sb.append("}");
        return sb.toString();
    }
    
    public static String toString(Object value) {
        if (value == null) {
            return null;
        }
        
        KNOWN_TYPE valueType = KNOWN_TYPE.resolveType(value);
        if (valueType == null) {
            throw new IllegalArgumentException("неизвестный тип поля");
        }
        
        switch (valueType) {
            case STRING:
                return (String) value;
            case INTEGER:                
            case LONG:                
            case BOOLEAN:
                return value.toString();
            case DATE:
                return DATE_FORMAT.format((java.util.Date) value);
            case LOCAL_DATE:
                return ((java.time.LocalDate) value).format(LOCAL_DATE_FORMAT);
            case LOCAL_DATE_TIME:
                return ((java.time.LocalDateTime) value).format(LOCAL_DATE_TIME_FORMAT);
            case ENUM:
                return ((Enum) value).name();
            case ENTITY:
                return EntityConverterHelper.getEntityId(value);
            case COLLECTION:
                return getCollectionAsString((Collection) value);
        }
        
        return null;
    }

    @SuppressWarnings("unchecked")
    public static <T> T parseValue(String value, Class<T> type) {
        if (value == null || type == null) {
            return null;
        }
        if (type.equals(String.class)) {
            return (T) value;
        }
        else if (type.equals(Long.class)) {
            return (T) (Long) Long.parseLong(value);
        }
        else if (type.equals(Integer.class)) {
            return (T) (Integer) Integer.parseInt(value);
        }
        else if (type.equals(Boolean.class)) {
            return (T) (Boolean) Boolean.parseBoolean(value);
        }
        else if (type.equals(java.time.LocalDateTime.class)) {
            return (T) java.time.LocalDateTime.parse(value, LOCAL_DATE_TIME_FORMAT);
        }
        else if (type.equals(java.time.LocalDate.class)) {
            return (T) java.time.LocalDate.parse(value, LOCAL_DATE_FORMAT);
        }
        else {
            throw new IllegalArgumentException("Incorrect field type " + type);
        }
    }

    /**
     * Текстовое представление коллекции.
     * [значение1,значение2]
     * 
     * @param value
     * @return 
     */
    public static String getCollectionAsString(Collection value) {
        StringJoiner joiner = new StringJoiner(COLLECTION_DELIMITER);        
        value.forEach(v -> joiner.add(EntityConverterHelper.getEntityId(v)));
        return "[" + joiner.toString() + "]";
    }
    
    public static boolean equals(Object value1, Object value2) {
        if (value1 == null && value2 == null) {
            return true;
        }
        
        KNOWN_TYPE valueType = KNOWN_TYPE.resolveType(value1, value2);
        if (valueType == null) {
            throw new IllegalArgumentException("неизвестный тип поля");
        }
                
        switch (valueType) {
            case STRING:
                return equalsString((String) value1, (String) value2);
            case INTEGER:                
            case LONG:                
            case BOOLEAN:
            case DATE:
            case LOCAL_DATE:
            case LOCAL_DATE_TIME:
            case ENUM:
                return equalsTyped(value1, value2);
            case ENTITY:
                return equalsEntity(value1, value2);
            case COLLECTION:
                return equalsCollection((Collection) value1, (Collection) value2);
        }
        
        return false;
    }
    
    public static boolean equalsString(String value1, String value2) {
        if (Strings.isNullOrEmpty(value1) && Strings.isNullOrEmpty(value2)) {
            return true;
        }
        if (value1 != null) {
            return value1.equals(value2);
        }
        if (value2 != null) {
            return value2.equals(value1);
        }
        return false;
    }
    
    public static <T> boolean equalsTyped(T value1, T value2) {
        if (value1 == null && value2 == null) {
            return true;
        }
        if (value1 != null) {
            return value1.equals(value2);
        }
        if (value2 != null) {
            return value2.equals(value1);
        }
        return false;
    }
    
    public static <T> boolean equalsEntity(T value1, T value2) {
        if (value1 == null && value2 == null) {
            return true;
        }
        if (value1 != null) {
            return value1.equals(value2);
        }
        if (value2 != null) {
            return value2.equals(value1);
        }
        return false;
    }
    
    public static <T> boolean equalsCollection(Collection<T> value1, Collection<T> value2) {
        if (CollectionsUtil.isNullOrEmpty(value1) && CollectionsUtil.isNullOrEmpty(value2)) {
            return true;
        }
        if (CollectionsUtil.size(value1) != CollectionsUtil.size(value2)) {
            return false;
        }
        
        HashSet<T> col1 = new HashSet<>();
        if (value1 != null) col1.addAll(value1);
        HashSet<T> col2 = new HashSet<>();
        if (value2 != null) col2.addAll(value2);
        
        return col1.containsAll(col2);
    }

    public static Method getSetterMethod(Class objectType, Field f) {
        String methodName = null;
        try {
            methodName = "set" + f.getName().substring(0, 1).toUpperCase() + f.getName().substring(1);
            return objectType.getMethod(methodName, f.getType());
        } catch (Exception e) {
            LOG.error("Отсутствует метод: " + methodName + " в классе: " + objectType.getName(), e);
            throw new RuntimeException(e);
        }
    }

    public static enum KNOWN_TYPE {
        STRING,
        INTEGER,
        LONG,
        BOOLEAN,
        DATE,
        LOCAL_DATE,
        LOCAL_DATE_TIME,
        COLLECTION,
        ENUM,
        ENTITY;
        
        KNOWN_TYPE() {}
        
        public static KNOWN_TYPE resolveType(Object obj) {
            if (obj == null) {
                return null;
            }
            if (obj instanceof String) {
                return STRING;
            } else if (obj instanceof Integer) {
                return INTEGER;
            } else if (obj instanceof Long) {
                return LONG;
            } else if (obj instanceof Boolean) {
                return BOOLEAN;
            } else if (obj instanceof java.util.Date) {
                return DATE;
            } else if (obj instanceof java.time.LocalDate) {
                return LOCAL_DATE;
            } else if (obj instanceof java.time.LocalDateTime) {
                return LOCAL_DATE_TIME;
            } else if (obj instanceof Collection) {
                return COLLECTION;
            } else if (obj.getClass().isEnum()) {
                return ENUM;
            } else if (obj.getClass().getAnnotation(Entity.class) != null) {
                return ENTITY;
            }
            return null;
        }
        
        public static KNOWN_TYPE resolveType(Object obj1, Object obj2) {
            if (obj1 != null) {
                return resolveType(obj1);
            } else {
                return resolveType(obj2);
            }
        }
    }
}
