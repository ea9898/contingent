package moscow.ptnl.contingent.area.entity.area;

import moscow.ptnl.contingent.area.entity.nsi.AreaTypes;

import javax.persistence.Cacheable;
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
import java.util.Objects;

@Entity
@Table(name = "MU_PROFILES")
@SequenceGenerator(name = "SEQ_MU_PROFILES", sequenceName = "SEQ_MU_PROFILES", allocationSize=1)
@Cacheable
public class MuProfile implements Serializable {

    private static final long serialVersionUID = 8725459806418462906L;

    @Id
    @GeneratedValue(strategy= GenerationType.SEQUENCE, generator="SEQ_MU_PROFILES")
    @Column(name = "ID", unique = true, nullable = false)
    private Long id;

    @Column(name = "MU_ID", nullable = false)
    private Long muId;

    @JoinColumn(name = "AREA_TYPE_CODE", nullable = false)
    @ManyToOne(fetch = FetchType.LAZY)
    private AreaTypes areaType;

    public MuProfile() {}

    public MuProfile(Long muId, AreaTypes areaType) {
        this.muId = muId;
        this.areaType = areaType;
    }

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
        if (obj != null && obj instanceof MuProfile) {
            return ((MuProfile) obj).getId().equals(this.id);
        }
        return false;
    }

    @Override
    public int hashCode() {        
        return (this.id != null) ? Objects.hashCode(this.id) : 0;
    }
}
