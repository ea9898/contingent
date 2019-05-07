package moscow.ptnl.contingent.area.entity.nsi;

import moscow.ptnl.contingent.area.entity.converter.BooleanIntegerConverter;

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

@Entity
@Table(name = "MU_PROFILE_TEMPLATES")
@Cacheable
public class MUProfileTemplates implements Serializable {

    private static final long serialVersionUID = -6543198512394189025L;

    @Id
    @Column(name = "ID", unique = true, nullable = false)
    private Long id;

    @Column(name = "MU_TYPE_ID")
    private Integer muTypeId;

    @JoinColumn(name = "AREA_TYPE_CODE")
    @ManyToOne(fetch = FetchType.LAZY)
    private AreaTypes areaType;

    @Column(name = "AVAILABLE_TO_CREATE")
    @Convert(converter = BooleanIntegerConverter.class)
    private Boolean availableToCreate;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Integer getMuTypeId() {
        return muTypeId;
    }

    public void setMuTypeId(Integer muTypeId) {
        this.muTypeId = muTypeId;
    }

    public AreaTypes getAreaType() {
        return areaType;
    }

    public void setAreaType(AreaTypes areaType) {
        this.areaType = areaType;
    }

    public Boolean getAvailableToCreate() {
        return availableToCreate;
    }

    public void setAvailableToCreate(Boolean availableToCreate) {
        this.availableToCreate = availableToCreate;
    }
    
    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj != null && obj instanceof MUProfileTemplates) {
            return ((MUProfileTemplates) obj).getId().equals(this.id);
        }
        return false;
    }

    @Override
    public int hashCode() {        
        return Objects.hashCode(this.id);
    }
}
