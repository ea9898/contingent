package moscow.ptnl.contingent.area.entity.area;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import java.io.Serializable;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Objects;
import moscow.ptnl.contingent.area.entity.area.Addresses;
import moscow.ptnl.contingent.area.entity.area.MoAddress;

@Entity
@Table(name = "AREA_ADDRESSES")
public class AreaAddress implements Serializable {

    private static final long serialVersionUID = 5982770527711526102L;

    @Id
    @Column(name = "ID", unique = true, nullable = false)
    private Long id;

    @JoinColumn(name = "MO_ADDRESS_ID")
    @ManyToOne(fetch = FetchType.LAZY)
    private MoAddress moAddress;

    @JoinColumn(name = "AREA_ID")
    @ManyToOne(fetch = FetchType.LAZY)
    private Area area;

    @Column(name = "START_DATE")
    private LocalDate startDate;

    @Column(name = "END_DATE")
    private LocalDate endDate;

    @JoinColumn(name = "ADDRESS_ID")
    @ManyToOne(fetch = FetchType.LAZY)
    private Addresses address;

    @Column(name = "CREATE_DATE", nullable = false)
    private LocalDateTime createDate;

    @Column(name = "UPDATE_DATE")
    private LocalDateTime updateDate;

    public Area getArea() {
        return area;
    }

    public void setArea(Area area) {
        this.area = area;
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

    public Addresses getAddress() {
        return address;
    }

    public void setAddress(Addresses address) {
        this.address = address;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public MoAddress getMoAddress() {
        return moAddress;
    }

    public void setMoAddress(MoAddress moAddress) {
        this.moAddress = moAddress;
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

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj != null && obj instanceof AreaAddress) {
            return ((AreaAddress) obj).getId().equals(this.id);
        }
        return false;
    }

    @Override
    public int hashCode() {        
        return Objects.hashCode(this.id);
    }
}
