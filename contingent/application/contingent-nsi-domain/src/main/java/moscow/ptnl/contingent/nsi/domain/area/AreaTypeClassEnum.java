package moscow.ptnl.contingent.nsi.domain.area;

import java.util.Objects;

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

    public boolean areaTypeClassEquals(AreaTypeClass areaTypeClass) {
        return areaTypeClass != null && Objects.equals(clazz, areaTypeClass.getCode());
    }
}
