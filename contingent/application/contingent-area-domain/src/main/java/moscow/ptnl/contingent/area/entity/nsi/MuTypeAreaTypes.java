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
import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import java.io.Serializable;
import java.util.Objects;

import moscow.ptnl.contingent.area.entity.converter.BooleanStrictIntegerConverter;

@Entity
@Table(name = "MU_TYPE_AREA_TYPES")
@Cacheable
public class MuTypeAreaTypes implements Serializable {

    private static final long serialVersionUID = -6543198512394189025L;

    @Id
    @Column(name = "ID", unique = true, nullable = false)
    private Long id;

    @Column(name = "MU_TYPE_CODE")
    private Integer muTypeCode;

    @JoinColumn(name = "AREA_TYPE_CODE")
    @ManyToOne(fetch = FetchType.LAZY)
    private AreaType areaType;

    @Column(name = "AVAILABLE_TO_CREATE")
    @Min(1) @Max(2)
    private Integer availableToCreate;

    @Column(name = "ARCHIVED", nullable = false)
    @Convert(converter = BooleanStrictIntegerConverter.class)
    private Boolean archived;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Integer getMuTypeCode() {
        return muTypeCode;
    }

    public void setMuTypeCode(Integer muTypeCode) {
        this.muTypeCode = muTypeCode;
    }

    public AreaType getAreaType() {
        return areaType;
    }

    public void setAreaType(AreaType areaType) {
        this.areaType = areaType;
    }

    public Integer getAvailableToCreate() {
        return availableToCreate;
    }

    public void setAvailableToCreate(Integer availableToCreate) {
        this.availableToCreate = availableToCreate;
    }
    
    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj != null && obj instanceof MuTypeAreaTypes) {
            return ((MuTypeAreaTypes) obj).getId().equals(this.id);
        }
        return false;
    }

    @Override
    public int hashCode() {        
        return Objects.hashCode(this.id);
    }
}
