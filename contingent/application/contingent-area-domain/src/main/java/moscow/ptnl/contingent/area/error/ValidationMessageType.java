package moscow.ptnl.contingent.area.error;

/**
 * Информация по валидации
 */
public enum ValidationMessageType{
    ERROR,
    WARNING,
    INFO;

    public String value() {
        return name();
    }

    public static ValidationMessageType fromValue(String v) {
        return valueOf(v);
    }
}
