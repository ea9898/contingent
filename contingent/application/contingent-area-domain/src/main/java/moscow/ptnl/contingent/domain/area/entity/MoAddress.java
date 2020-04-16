package moscow.ptnl.contingent.domain.area.entity;

import moscow.ptnl.contingent.domain.history.ServiceName;
import moscow.ptnl.contingent.domain.history.meta.Journalable;
import moscow.ptnl.contingent.domain.history.meta.LogIt;
import moscow.ptnl.contingent.nsi.domain.area.AreaType;
import org.hibernate.annotations.Proxy;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import java.io.Serializable;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Objects;

@Entity @Journalable(ServiceName.AREA)
@Proxy(lazy=false)
@Table(name = "MO_ADDRESSES")
@SequenceGenerator(name = "SEQ_MO_ADDRESSES", sequenceName = "SEQ_MO_ADDRESSES", allocationSize=1)
public class MoAddress implements Serializable {

    private static final long serialVersionUID = -786212687627911093L;

    @Id
    @GeneratedValue(strategy= GenerationType.SEQUENCE, generator="SEQ_MO_ADDRESSES")
    @Column(name = "ID", unique = true, nullable = false)
    private Long id;

    @LogIt
    @Column(name = "MO_ID")
    private Long moId;

    @LogIt
    @JoinColumn(name = "AREA_TYPE_CODE")
    @ManyToOne(fetch = FetchType.LAZY)
    private AreaType areaType;

    @LogIt
    @JoinColumn(name = "ORDER_ID")
    @ManyToOne(fetch = FetchType.LAZY)
    private AddressAllocationOrders addressAllocationOrder;

    @LogIt
    @Column(name = "START_DATE")
    private LocalDate startDate;

    @LogIt
    @Column(name = "END_DATE")
    private LocalDate endDate;

    @LogIt
    @JoinColumn(name = "REJECT_ORDER_ID")
    @ManyToOne(fetch = FetchType.LAZY)
    private AddressAllocationOrders addressRejectOrder;

    @LogIt
    @Column(name = "CREATE_DATE", nullable = false)
    private LocalDateTime createDate;

    @LogIt
    @Column(name = "UPDATE_DATE")
    private LocalDateTime updateDate;

    @LogIt
    @JoinColumn(name = "ADDRESS_ID")
    @ManyToOne(fetch = FetchType.LAZY)
    private Addresses address;

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

    public AreaType getAreaType() {
        return areaType;
    }

    public void setAreaType(AreaType areaType) {
        this.areaType = areaType;
    }

    public AddressAllocationOrders getAddressAllocationOrder() {
        return addressAllocationOrder;
    }

    public void setAddressAllocationOrder(AddressAllocationOrders addressAllocationOrder) {
        this.addressAllocationOrder = addressAllocationOrder;
    }

    public LocalDate getStartDate() {
        return startDate;
    }

    public void setStartDate(LocalDate startDate) {
        this.startDate = startDate;
    }

    public LocalDate getEndDate() {
        return endDate;
    }

    public void setEndDate(LocalDate endDate) {
        this.endDate = endDate;
    }

    public AddressAllocationOrders getAddressRejectOrder() {
        return addressRejectOrder;
    }

    public void setAddressRejectOrder(AddressAllocationOrders addressRejectOrder) {
        this.addressRejectOrder = addressRejectOrder;
    }

    public LocalDateTime getCreateDate() {
        return createDate;
    }

    public void setCreateDate(LocalDateTime createDate) {
        this.createDate = createDate;
    }

    public LocalDateTime getUpdateDate() {
        return updateDate;
    }

    public void setUpdateDate(LocalDateTime updateDate) {
        this.updateDate = updateDate;
    }

    public Addresses getAddress() {
        return address;
    }

    public void setAddress(Addresses address) {
        this.address = address;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj != null && obj instanceof MoAddress) {
            return ((MoAddress) obj).getId().equals(this.id);
        }
        return false;
    }

    @Override
    public int hashCode() {        
        return Objects.hashCode(this.id);
    }
}
