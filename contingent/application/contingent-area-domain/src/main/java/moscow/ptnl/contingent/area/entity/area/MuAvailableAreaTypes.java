package moscow.ptnl.contingent.area.entity.area;

import moscow.ptnl.contingent.area.entity.nsi.AreaType;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.Objects;

@Entity
@Table(name = "MU_AVAILABLE_AREA_TYPES")
public class MuAvailableAreaTypes implements Serializable {

    private static final long serialVersionUID = 7163458434444617867L;

    @Id
    @Column(name = "ID", unique = true, nullable = false)
    private Long id;

    @Column(name = "MU_ID")
    private Long muId;

    @JoinColumn(name = "AREA_TYPE_CODE")
    @ManyToOne(fetch = FetchType.LAZY)
    private AreaType areaType;

    @JoinColumn(name = "MO_AVAILABLE_AREA_TYPES_ID")
    @ManyToOne(fetch = FetchType.LAZY)
    private MoAvailableAreaTypes moAvailableAreaType;

    @Column(name = "CREATE_DATE", nullable = false)
    private LocalDateTime createDate;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getMuId() {
        return muId;
    }

    public void setMuId(Long muId) {
        this.muId = muId;
    }

    public AreaType getAreaType() {
        return areaType;
    }

    public void setAreaType(AreaType areaType) {
        this.areaType = areaType;
    }

    public MoAvailableAreaTypes getMoAvailableAreaType() {
        return moAvailableAreaType;
    }

    public void setMoAvailableAreaType(MoAvailableAreaTypes moAvailableAreaType) {
        this.moAvailableAreaType = moAvailableAreaType;
    }

    public LocalDateTime getCreateDate() {
        return createDate;
    }

    public void setCreateDate(LocalDateTime createDate) {
        this.createDate = createDate;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj != null && obj instanceof MuAvailableAreaTypes) {
            return ((MuAvailableAreaTypes) obj).getId().equals(this.id);
        }
        return false;
    }

    @Override
    public int hashCode() {        
        return Objects.hashCode(this.id);
    }
}
