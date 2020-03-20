package moscow.ptnl.contingent.nsi.domain.area;

/**
 * Тип полиса
 */
public enum PolicyTypeEnum {

    OMS(1);

    private final long code;

    PolicyTypeEnum(long code) {
        this.code = code;
    }

    public long getCode() {
        return code;
    }

}
