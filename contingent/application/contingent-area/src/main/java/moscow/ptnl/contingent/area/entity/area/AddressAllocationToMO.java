package moscow.ptnl.contingent.area.entity.area;

import moscow.ptnl.contingent.area.entity.nsi.AddressFormingElement;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypes;
import moscow.ptnl.contingent.area.entity.nsi.RegistryBuilding;

import javax.persistence.Cacheable;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.validation.constraints.Size;
import java.io.Serializable;
import java.util.Date;

@Entity
@Table(name = "ADDRESS_ALLOCATION_TO_MO")
@Cacheable
public class AddressAllocationToMO implements Serializable {

    private static final long serialVersionUID = -786212687627911093L;

    @Id
	@Column(name = "ID", unique = true, nullable = false)
	private Long id;

	@Column(name = "MO_ID")
	private Long moId;

    @JoinColumn(name = "AREA_TYPE_CODE")
    @ManyToOne(fetch = FetchType.LAZY)
    private AreaTypes areaType;

    @Column(name = "ORDER_ID")
    private Long orderId;

    @Column(name = "START_DATE")
    private Date startDate;

    @Column(name = "END_DATE")
    private Date endDate;

    @Column(name = "REJECT_ORDER_ID")
    private Long rejectOrderId;

    @Size(max = 255)
    @Column(name = "ARCHIVE_REASON")
    private String archiveReason;

    @JoinColumn(name = "ADDRESS_FORMING_ELEMENT_ID")
    @ManyToOne(fetch = FetchType.LAZY)
    private AddressFormingElement addressFormingElement;

    @JoinColumn(name = "REGISTRY_BUILDING_ID")
    @ManyToOne(fetch = FetchType.LAZY)
    private RegistryBuilding registryBuilding;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getMoId() {
        return moId;
    }

    public void setMoId(Long moId) {
        this.moId = moId;
    }

    public AreaTypes getAreaType() {
        return areaType;
    }

    public void setAreaType(AreaTypes areaType) {
        this.areaType = areaType;
    }

    public Long getOrderId() {
        return orderId;
    }

    public void setOrderId(Long orderId) {
        this.orderId = orderId;
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

    public Long getRejectOrderId() {
        return rejectOrderId;
    }

    public void setRejectOrderId(Long rejectOrderId) {
        this.rejectOrderId = rejectOrderId;
    }

    public String getArchiveReason() {
        return archiveReason;
    }

    public void setArchiveReason(String archiveReason) {
        this.archiveReason = archiveReason;
    }

    public AddressFormingElement getAddressFormingElement() {
        return addressFormingElement;
    }

    public void setAddressFormingElement(AddressFormingElement addressFormingElement) {
        this.addressFormingElement = addressFormingElement;
    }

    public RegistryBuilding getRegistryBuilding() {
        return registryBuilding;
    }

    public void setRegistryBuilding(RegistryBuilding registryBuilding) {
        this.registryBuilding = registryBuilding;
    }
}