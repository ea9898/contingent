package moscow.ptnl.contingent.area.model.esu;

public enum OperationType {

    UPDATE("update"), CREATE("create");

    private final String value;

    OperationType(String value) {
        this.value = value;
    }

    public String getValue() {
        return value;
    }
}
