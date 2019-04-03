package moscow.ptnl.contingent.area.entity.area;

import javax.persistence.Cacheable;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import java.io.Serializable;
import java.util.Date;

@Entity
@Table(name = "ADDRESS_ALLOCATION_TO_AREA")
@Cacheable
public class AddressAllocationToArea implements Serializable {

    private static final long serialVersionUID = 5982770527711526102L;

    @Id
    @JoinColumn(name = "DISTRIBUTION_TO_MO_ID")
    @ManyToOne(fetch = FetchType.LAZY)
	private AddressAllocationToMO distributionToMo;

    @Id
    @JoinColumn(name = "AREA_ID")
    @ManyToOne(fetch = FetchType.LAZY)
    private Area area;

    @Column(name = "START_DATE")
    private Date startDate;

    @Column(name = "END_DATE")
    private Date endDate;

    public AddressAllocationToMO getDistributionToMo() {
        return distributionToMo;
    }

    public void setDistributionToMo(AddressAllocationToMO distributionToMo) {
        this.distributionToMo = distributionToMo;
    }

    public Area getArea() {
        return area;
    }

    public void setArea(Area area) {
        this.area = area;
    }

    public Date getStartDate() {
        return startDate;
    }

    public void setStartDate(Date startDate) {
        this.startDate = startDate;
    }

    public Date getEndDate() {
        return endDate;
    }

    public void setEndDate(Date endDate) {
        this.endDate = endDate;
    }
}
