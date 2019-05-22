package moscow.ptnl.contingent.area.entity.nsi;

import javax.persistence.Cacheable;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import java.io.Serializable;
import java.util.Objects;

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
    private AreaTypes areaType;

    @JoinColumn(name = "POSITION_NOM_ID")
    @ManyToOne(fetch = FetchType.LAZY)
    private PositionNomClinic positionNomClinic;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public AreaTypes getAreaType() {
        return areaType;
    }

    public void setAreaType(AreaTypes areaType) {
        this.areaType = areaType;
    }

    public PositionNomClinic getPositionNomClinic() {
        return positionNomClinic;
    }

    public void setPositionNomClinic(PositionNomClinic positionNomClinic) {
        this.positionNomClinic = positionNomClinic;
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
