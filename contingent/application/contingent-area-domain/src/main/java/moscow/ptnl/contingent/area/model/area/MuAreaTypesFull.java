package moscow.ptnl.contingent.area.model.area;

import moscow.ptnl.contingent.nsi.domain.area.AreaType;

import java.util.List;

public class MuAreaTypesFull {

    private final List<AreaType> usedAreaTypes;

    private final List<AreaType> availableAreaTypes;

    public MuAreaTypesFull(List<AreaType> usedAreaTypes, List<AreaType> availableAreaTypes) {
        this.usedAreaTypes = usedAreaTypes;
        this.availableAreaTypes = availableAreaTypes;
    }

    public List<AreaType> getUsedAreaTypes() {
        return usedAreaTypes;
    }

    public List<AreaType> getAvailableAreaTypes() {
        return availableAreaTypes;
    }
}
