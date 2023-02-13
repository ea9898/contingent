package ru.mos.emias.errors.domain;

public enum ErrorMessageType {
    
    ERROR,
    WARNING,
    INFO;
    
    public String value() {
        return name();
    }
    
    public static ErrorMessageType fromValue(String v) {
        return valueOf(v);
    }
    
}
