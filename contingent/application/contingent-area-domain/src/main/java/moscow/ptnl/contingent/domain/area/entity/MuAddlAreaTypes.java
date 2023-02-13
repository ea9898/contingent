package moscow.ptnl.contingent.domain.area.entity;

import moscow.ptnl.contingent.nsi.domain.area.AreaType;
import org.hibernate.annotations.Proxy;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.SequenceGenerator;
import jakarta.persistence.Table;
import java.io.Serializable;
import java.util.Objects;

@Entity
@Table(name = "MU_ADDL_AREA_TYPES")
@Proxy(lazy=false)
@SequenceGenerator(name = "SEQ_MU_ADDL_AREA_TYPES", sequenceName = "SEQ_MU_ADDL_AREA_TYPES", allocationSize=1)
public class MuAddlAreaTypes implements Serializable {

    private static final long serialVersionUID = 8725459806418462906L;

    @Id
    @GeneratedValue(strategy= GenerationType.SEQUENCE, generator="SEQ_MU_ADDL_AREA_TYPES")
    @Column(name = "ID", unique = true, nullable = false)
    private Long id;

    @Column(name = "MU_ID", nullable = false)
    private Long muId;

    @JoinColumn(name = "AREA_TYPE_CODE")
    @ManyToOne(fetch = FetchType.LAZY)
    private AreaType areaType;

    public MuAddlAreaTypes() {}

    public MuAddlAreaTypes(Long muId, AreaType areaType) {
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

    public AreaType getAreaType() {
        return areaType;
    }

    public void setAreaType(AreaType areaType) {
        this.areaType = areaType;
    }
    
    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj != null && obj instanceof MuAddlAreaTypes) {
            return ((MuAddlAreaTypes) obj).getId().equals(this.id);
        }
        return false;
    }

    @Override
    public int hashCode() {        
        return Objects.hashCode(this.id);
    }
}
