package moscow.ptnl.contingent.area.entity.nsi;

import javax.persistence.Cacheable;
import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import java.io.Serializable;
import java.util.Objects;

import moscow.ptnl.contingent.area.entity.converter.BooleanStrictIntegerConverter;

@Entity
@Table(name = "AREA_TYPE_MEDICAL_POSITIONS")
@Cacheable
public class AreaTypeMedicalPositions implements Serializable {

    private static final long serialVersionUID = 4979698890748802824L;

    @Id
    @Column(name = "ID", unique = true, nullable = false)
    private Long id;

    @JoinColumn(name = "AREA_TYPE_CODE")
    @ManyToOne(fetch = FetchType.LAZY)
    private AreaType areaType;

    @JoinColumn(name = "POSITION_CODE")
    @ManyToOne(fetch = FetchType.LAZY)
    private PositionNom positionNom;

    @Column(name = "ARCHIVED")
    @Convert(converter = BooleanStrictIntegerConverter.class)
    private Boolean archived;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public AreaType getAreaType() {
        return areaType;
    }

    public void setAreaType(AreaType areaType) {
        this.areaType = areaType;
    }

    public PositionNom getPositionNom() {
        return positionNom;
    }

    public void setPositionNom(PositionNom positionNom) {
        this.positionNom = positionNom;
    }

    public Boolean getArchived() {
        return archived;
    }

    public void setArchived(Boolean archived) {
        this.archived = archived;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj != null && obj instanceof AreaTypeMedicalPositions) {
            return ((AreaTypeMedicalPositions) obj).getId().equals(this.id);
        }
        return false;
    }

    @Override
    public int hashCode() {        
        return Objects.hashCode(this.id);
    }
}
