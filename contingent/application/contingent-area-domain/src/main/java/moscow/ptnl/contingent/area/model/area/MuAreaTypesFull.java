package moscow.ptnl.contingent.area.model.area;

import moscow.ptnl.contingent.area.entity.nsi.AreaType;

import java.util.List;

public class MuAreaTypesFull {

    private List<AreaType> usedAreaTypes;

    private List<AreaType> availableAreaTypes;

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
