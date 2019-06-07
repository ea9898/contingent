package moscow.ptnl.contingent.domain.history.converter;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.text.SimpleDateFormat;
import java.time.format.DateTimeFormatter;
import java.util.Collection;
import java.util.HashSet;
import java.util.StringJoiner;
import javax.persistence.Entity;
import moscow.ptnl.contingent.domain.history.meta.FieldConverter;
import moscow.ptnl.util.CollectionsUtil;
import moscow.ptnl.util.Strings;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Конвертер для работы с популярными типами данных.
 * 
 * @author m.kachalov
 */
public class DefaultConverter implements FieldConverter {
    
    private final static Logger LOG = LoggerFactory.getLogger(DefaultConverter.class);
    
    public static final String COLLECTION_DELIMITER = ",";
    public static final String COMPOSITE_KEY_DELIMITER = ",";
    private final static SimpleDateFormat DATE_FORMAT = new SimpleDateFormat("dd.MM.yyyy HH:mm:ss");
    private final static DateTimeFormatter LOCAL_DATE_FORMAT = DateTimeFormatter.ofPattern("dd.MM.yyyy HH:mm:ss");

    @Override
    public boolean equals(Object value1, Object value2) {
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

    @Override
    public String toString(Object value) {
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
                return ((java.time.LocalDateTime) value).format(LOCAL_DATE_FORMAT);
            case ENUM:
                return ((Enum) value).name();
            case ENTITY:
                return getEntityId(value);
            case COLLECTION:
                return getCollectionAsString((Collection) value);
        }
        
        return null;
    }
    
    private boolean equalsString(String value1, String value2) {
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
    
    private <T> boolean equalsTyped(T value1, T value2) {
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
    
    private <T> boolean equalsEntity(T value1, T value2) {
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
    
    private <T> boolean equalsCollection(Collection<T> value1, Collection<T> value2) {
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
    
    private String getEntityId(Object entity) {
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
    private String getKeyAsString(Object key) {
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
    
    /**
     * Текстовое представление коллекции.
     * [значение1,значение2]
     * 
     * @param value
     * @return 
     */
    private String getCollectionAsString(Collection value) {
        StringJoiner joiner = new StringJoiner(COLLECTION_DELIMITER);        
        value.forEach(v -> joiner.add(getEntityId(v)));
        return "[" + joiner.toString() + "]";
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
