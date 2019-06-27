package moscow.ptnl.contingent.area.entity.nsi;

/**
 * Класс участка
 */
public enum AreaTypeClassEnum {

    PRIMARY(1),
    OTHER(2);

    private final long clazz;

    AreaTypeClassEnum(long clazz) {
        this.clazz = clazz;
    }

    public long getClazz() {
        return clazz;
    }

}
