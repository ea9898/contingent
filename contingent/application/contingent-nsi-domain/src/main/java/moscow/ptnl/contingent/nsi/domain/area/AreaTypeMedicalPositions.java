package moscow.ptnl.contingent.nsi.domain.area;

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
import moscow.ptnl.contingent.domain.Keyable;
import moscow.ptnl.contingent.domain.converter.BooleanStrictIntegerConverter;
import moscow.ptnl.contingent.nsi.domain.NsiTablesEnum;
import moscow.ptnl.contingent.nsi.domain.annotation.MapToNsi;


@Entity
@Table(name = "AREA_TYPE_MEDICAL_POSITIONS")
@Cacheable
@MapToNsi(table = NsiTablesEnum.AREA_TYPE_MEDICAL_POSITIONS)
public class AreaTypeMedicalPositions implements Serializable, Keyable {

    private static final long serialVersionUID = 4979698890748802824L;

    @Id
    @Column(name = "GLOBAL_ID", unique = true, nullable = false)
    @MapToNsi("GLOBAL_ID")
    private Long globalId;

    @JoinColumn(name = "AREA_TYPE_CODE")
    @ManyToOne(fetch = FetchType.LAZY)
    @MapToNsi(value = "AREA_TYPE_CODE", findEntityByField = "globalId")
    private AreaType areaType;

    @JoinColumn(name = "POSITION_CODE")
    @ManyToOne(fetch = FetchType.LAZY)
    @MapToNsi(value = "POSITION_CODE", findEntityByField = "positionCode", crossObject = PositionNom.class, crossField = "globalId")
    private PositionCode positionCode;
    
    @Column(name = "ARCHIVED")
    @Convert(converter = BooleanStrictIntegerConverter.class)
    @MapToNsi
    private Boolean archived;

    public Long getGlobalId() {
        return globalId;
    }

    public void setGlobalId(Long globalId) {
        this.globalId = globalId;
    }

    public AreaType getAreaType() {
        return areaType;
    }

    public void setAreaType(AreaType areaType) {
        this.areaType = areaType;
    }

    public Boolean getArchived() {
        return archived;
    }

    public void setArchived(Boolean archived) {
        this.archived = archived;
    }

    public PositionCode getPositionCode() {
        return positionCode;
    }

    public void setPositionCode(PositionCode positionCode) {
        this.positionCode = positionCode;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj instanceof AreaTypeMedicalPositions) {
            return ((AreaTypeMedicalPositions) obj).getGlobalId().equals(this.globalId);
        }
        return false;
    }

    @Override
    public int hashCode() {        
        return Objects.hashCode(this.globalId);
    }

    @Override
    public Serializable getKey() {
        return getGlobalId();
    }
}
