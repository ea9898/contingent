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

    @Column(name = "MEDICAL_POSITION_ID")
    private Long medicalPositionId;

    @JoinColumn(name = "AREA_TYPE_CODE")
    @ManyToOne(fetch = FetchType.LAZY)
    private AreaTypes areaType;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getMedicalPositionId() {
        return medicalPositionId;
    }

    public void setMedicalPositionId(Long medicalPositionId) {
        this.medicalPositionId = medicalPositionId;
    }

    public AreaTypes getAreaType() {
        return areaType;
    }

    public void setAreaType(AreaTypes areaType) {
        this.areaType = areaType;
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
        return (this.id != null) ? Objects.hashCode(this.id) : 0;
    }
}
