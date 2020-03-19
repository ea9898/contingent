package moscow.ptnl.contingent.domain.area.model.params;

import java.util.List;

public class MoAvailableAreaTypesParams {

    private long moId;

    private List<Long> areaTypeCodes;

    public long getMoId() {
        return moId;
    }

    public void setMoId(long moId) {
        this.moId = moId;
    }

    public List<Long> getAreaTypeCodes() {
        return areaTypeCodes;
    }

    public void setAreaTypeCodes(List<Long> areaTypeCodes) {
        this.areaTypeCodes = areaTypeCodes;
    }
}
