package moscow.ptnl.contingent.area.entity.area;

import moscow.ptnl.contingent.area.entity.nsi.AreaTypes;

import javax.persistence.Cacheable;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import java.io.Serializable;

@Entity
@Table(name = "MU_PROFILES")
@Cacheable
public class MuProfile implements Serializable {

    private static final long serialVersionUID = 8725459806418462906L;

    @Id
	@Column(name = "MU_ID", unique = true, nullable = false)
	private Long muId;

    @JoinColumn(name = "AREA_TYPE_CODE", nullable = false)
    @ManyToOne(fetch = FetchType.LAZY)
    private AreaTypes areaType;

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
}
