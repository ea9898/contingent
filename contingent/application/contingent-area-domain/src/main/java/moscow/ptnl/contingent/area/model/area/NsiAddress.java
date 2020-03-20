package moscow.ptnl.contingent.area.model.area;

import java.util.Objects;

public class NsiAddress {

    private Long globalId;

    private Integer levelAddress;

    public Long getGlobalId() {
        return globalId;
    }

    public void setGlobalId(Long globalId) {
        this.globalId = globalId;
    }

    public Integer getLevelAddress() {
        return levelAddress;
    }

    public void setLevelAddress(Integer levelAddress) {
        this.levelAddress = levelAddress;
    }

    public NsiAddress() {
    }

    public NsiAddress(Long globalId, Integer levelAddress) {
        this.globalId = globalId;
        this.levelAddress = levelAddress;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        NsiAddress that = (NsiAddress) o;
        return Objects.equals(globalId, that.globalId) &&
                Objects.equals(levelAddress, that.levelAddress);
    }

    @Override
    public int hashCode() {
        return Objects.hash(globalId, levelAddress);
    }
}
