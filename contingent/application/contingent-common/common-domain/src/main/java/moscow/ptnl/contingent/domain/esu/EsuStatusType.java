package moscow.ptnl.contingent.domain.esu;

/**
 * 0 - не успешно, 
 * 1 - успешно, 
 * 2 - в процессе обработки, 
 * 3 - не планируются к дальнейшей отправке
 */
public enum EsuStatusType {

    UNSUCCESS(0),
    SUCCESS(1),
    INPROGRESS(2),
    /** 
     * Техническое обслуживание (например тестирование), сообщения не должны 
     * попадать в Kafka. 
     * Используется при настройке esu_output_sending_enabled = 0.
     */
    ТО_STATUS(3);

    private final Integer value;

    EsuStatusType(Integer value) {
        this.value = value;
    }

    public Integer getValue() {
        return this.value;
    }

    public static EsuStatusType getByValue(Integer value) {
        if (value == null)
            return null;
        for (EsuStatusType status : EsuStatusType.values()) {
            if (status.getValue().equals(value))
                return status;
        }
        return null;
    }

}
