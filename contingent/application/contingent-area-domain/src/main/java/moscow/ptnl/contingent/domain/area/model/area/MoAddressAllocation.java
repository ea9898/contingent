package moscow.ptnl.contingent.domain.area.model.area;

import moscow.ptnl.contingent.nsi.domain.area.AreaType;

/**
 * @author sorlov
 */
public class MoAddressAllocation {

    public final Long addressGlobalId;
    public final Long moId;
    public final AreaType areaType;
    public final long moAddressId;

    public MoAddressAllocation(Long addressGlobalId, Long moId, AreaType areaType, Long moAddressId) {
        this.addressGlobalId = addressGlobalId;
        this.moId = moId;
        this.areaType = areaType;
        this.moAddressId = moAddressId;
    }
}
