package moscow.ptnl.contingent.area.entity.area;

import moscow.ptnl.contingent.area.entity.nsi.AreaType;

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
import java.time.LocalDateTime;
import java.util.Objects;

@Entity
@Table(name = "MU_AVAILABLE_AREA_TYPES")
@SequenceGenerator(name = "SEQ_MU_AVAILABLE_AREA_TYPES", sequenceName = "SEQ_MU_AVAILABLE_AREA_TYPES", allocationSize=1)
public class MuAvailableAreaTypes implements Serializable {

    private static final long serialVersionUID = -1737065823045335993L;

    @Id
    @GeneratedValue(strategy= GenerationType.SEQUENCE, generator="SEQ_MU_AVAILABLE_AREA_TYPES")
    @Column(name = "ID", unique = true, nullable = false)
    private Long id;

    @Column(name = "MU_ID")
    private Long moId;

    @JoinColumn(name = "AREA_TYPE_CODE")
    @ManyToOne(fetch = FetchType.LAZY)
    private AreaType areaType;

    @JoinColumn(name = "MO_AVAILABLE_AREA_TYPES_ID")
    @ManyToOne(fetch = FetchType.LAZY)
    private MoAvailableAreaTypes moAvailableAreaTypes;

    @Column(name = "CREATE_DATE", nullable = false)
    private LocalDateTime createDate;

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

    public MoAvailableAreaTypes getMoAvailableAreaTypes() {
        return moAvailableAreaTypes;
    }

    public void setMoAvailableAreaTypes(MoAvailableAreaTypes moAvailableAreaTypes) {
        this.moAvailableAreaTypes = moAvailableAreaTypes;
    }

    public LocalDateTime getCreateDate() {
        return createDate;
    }

    public void setCreateDate(LocalDateTime createDate) {
        this.createDate = createDate;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        MuAvailableAreaTypes that = (MuAvailableAreaTypes) o;
        return Objects.equals(id, that.id) &&
                Objects.equals(moId, that.moId) &&
                Objects.equals(areaType, that.areaType) &&
                Objects.equals(moAvailableAreaTypes, that.moAvailableAreaTypes) &&
                Objects.equals(createDate, that.createDate);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id, moId, areaType, moAvailableAreaTypes, createDate);
    }
}
