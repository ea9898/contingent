/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package moscow.ptnl.contingent.area.entity.settings;

import java.text.SimpleDateFormat;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;

/**
 * Типы системных настроек из AD_CONFIG.
 * 
 * @author m.kachalov
 */
public enum SettingValueType {
    
    LONG(1),
    /** Дата в формате "yyyy-MM-dd" */
    DATE(2),
    STRING(3),
    /** Время в формате "00:00:00" */
    TIME(4),
    /** 1 = TRUE, все остальное, кроме null, это FALSE */
    BOOLEAN(5);
    
    private static final SimpleDateFormat DATE_FORMAT = new SimpleDateFormat("yyyy-MM-dd");
    private static final DateTimeFormatter TIME_FORMAT = DateTimeFormatter.ofPattern("HH:mm:ss");
    
    private final long typeCode;
    
    SettingValueType(long typeCode) {
        this.typeCode = typeCode;
    }

    public Long getTypeCode() {
        return typeCode;
    }
    
    public static SettingValueType valueOf(Long typeCode) {
        for (SettingValueType t : SettingValueType.values()) {
            if (t.getTypeCode().equals(typeCode)) {
                return t;
            }
        }
        return null;
    }
    
    /**
     * Конвертирует строковое значение, приводя его к актуальному типу.
     *  
     * @param <T>
     * @param value
     * @return null, Long, Date, String, LocalTime
     * @throws Exception 
     */
    public <T> T typeValue(String value) throws Exception {
        if (value == null) {
            return null;
        }
        switch ((int) typeCode) {
            case 1: 
                return (T) (Long) Long.parseLong(value);
            case 2:
                return (T) DATE_FORMAT.parse(value);
            case 3:
                return (T) value;
            case 4:
                return (T) LocalTime.parse(value, TIME_FORMAT);
            case 5:
                return (T) (Boolean) "1".equals(value);
            default:
                throw new RuntimeException("Не поддерживаемый тип данных");
            
        }
    }
    
}
