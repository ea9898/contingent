package moscow.ptnl.contingent.area.entity.area;

import moscow.ptnl.contingent.nsi.domain.area.AreaType;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.Objects;
import java.util.Set;

@Entity
@Table(name = "MO_AVAILABLE_AREA_TYPES")
@SequenceGenerator(name = "SEQ_MO_AVAILABLE_AREA_TYPES", sequenceName = "SEQ_MO_AVAILABLE_AREA_TYPES", allocationSize=1)
public class MoAvailableAreaTypes implements Serializable {

    private static final long serialVersionUID = -1737065823045615993L;

    @Id
    @GeneratedValue(strategy= GenerationType.SEQUENCE, generator="SEQ_MO_AVAILABLE_AREA_TYPES")
    @Column(name = "ID", unique = true, nullable = false)
    private Long id;

    @Column(name = "MO_ID")
    private Long moId;

    @JoinColumn(name = "AREA_TYPE_CODE")
    @ManyToOne(fetch = FetchType.LAZY)
    private AreaType areaType;

    @Column(name = "CREATE_DATE", nullable = false)
    private LocalDateTime createDate;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "moAvailableAreaType")
    private Set<MuAvailableAreaTypes> muAvailableAreaTypes;

    public MoAvailableAreaTypes() {
    }

    public MoAvailableAreaTypes(Long moId, AreaType areaType, LocalDateTime createDate, Set<MuAvailableAreaTypes> muAvailableAreaTypes) {
        this.moId = moId;
        this.areaType = areaType;
        this.createDate = createDate;
        this.muAvailableAreaTypes = muAvailableAreaTypes;
    }


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

    public LocalDateTime getCreateDate() {
        return createDate;
    }

    public void setCreateDate(LocalDateTime createDate) {
        this.createDate = createDate;
    }

    public Set<MuAvailableAreaTypes> getMuAvailableAreaTypes() {
        return muAvailableAreaTypes;
    }

    public void setMuAvailableAreaTypes(Set<MuAvailableAreaTypes> muAvailableAreaTypes) {
        this.muAvailableAreaTypes = muAvailableAreaTypes;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof MoAvailableAreaTypes)) return false;

        MoAvailableAreaTypes that = (MoAvailableAreaTypes) o;

        if (id != null ? !id.equals(that.id) : that.id != null) return false;
        if (moId != null ? !moId.equals(that.moId) : that.moId != null) return false;
        if (areaType != null ? !areaType.equals(that.areaType) : that.areaType != null) return false;
        return muAvailableAreaTypes != null ? muAvailableAreaTypes.equals(that.muAvailableAreaTypes) : that.muAvailableAreaTypes == null;
    }

    @Override
    public int hashCode() {        
        return Objects.hashCode(this.id);
    }
}
