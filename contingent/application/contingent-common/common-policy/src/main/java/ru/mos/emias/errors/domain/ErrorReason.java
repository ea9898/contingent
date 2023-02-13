package ru.mos.emias.errors.domain;

public interface ErrorReason {
    
    String getDescription();

    String getCode();

    ErrorMessageType getMessageType();
    
}
