package moscow.ptnl.contingent.nsi.domain.area;

public enum SpecializationEnum {

    CHILD_ONCOLOGY(19),
    ONCOLOGY(41);

    private final long code;

    SpecializationEnum(long code) {
        this.code = code;
    }

    public long getCode() {
        return code;
    }

}
