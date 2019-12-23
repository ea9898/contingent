package moscow.ptnl.contingent.domain.esu;

//0-не успешно, 1-успешно, 2-в процессе обработки
public enum EsuStatusType {

    UNSUCCESS(0),
    SUCCESS(1),
    INPROGRESS(2);

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
