package moscow.ptnl.contingent.domain.area.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.SequenceGenerator;
import jakarta.persistence.Table;
import java.io.Serializable;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Objects;

import moscow.ptnl.contingent.domain.history.ServiceName;
import moscow.ptnl.contingent.domain.history.meta.Journalable;
import moscow.ptnl.contingent.domain.history.meta.LogIt;
import org.hibernate.annotations.Proxy;

@Entity @Journalable(ServiceName.AREA)
@Table(name = "AREA_ADDRESSES")
@Proxy(lazy=false)
@SequenceGenerator(name = "seq_area_address", sequenceName = "seq_area_address", allocationSize=1)
public class AreaAddress implements Serializable, Cloneable {

    private static final long serialVersionUID = 5982770527711526102L;

    @Id
    @GeneratedValue(strategy= GenerationType.SEQUENCE, generator="seq_area_address")
    @Column(name = "ID", unique = true, nullable = false)
    private Long id;

    @LogIt
    @JoinColumn(name = "MO_ADDRESS_ID")
    @ManyToOne(fetch = FetchType.LAZY)
    private MoAddress moAddress;

    @LogIt
    @JoinColumn(name = "AREA_ID")
    @ManyToOne(fetch = FetchType.LAZY)
    private Area area;

    @LogIt
    @Column(name = "START_DATE")
    private LocalDate startDate;

    @LogIt
    @Column(name = "END_DATE")
    private LocalDate endDate;

    @LogIt
    @JoinColumn(name = "ADDRESS_ID")
    @ManyToOne(fetch = FetchType.LAZY)
    private Addresses address;

    @LogIt
    @Column(name = "CREATE_DATE", nullable = false)
    private LocalDateTime createDate;

    @LogIt
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


    @Override
    public AreaAddress clone() {
        try {
            return (AreaAddress) super.clone();
        } catch (CloneNotSupportedException e) {
            return null;
        }
    }
}
