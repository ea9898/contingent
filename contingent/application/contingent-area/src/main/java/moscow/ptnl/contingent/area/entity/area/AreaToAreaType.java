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
@Table(name = "AREA_TO_AREA_TYPE")
@Cacheable
public class AreaToAreaType implements Serializable {

    private static final long serialVersionUID = 3586944486563837581L;

    @Id
	@Column(name = "ID", unique = true, nullable = false)
	private Long id;

    @JoinColumn(name = "AREA_ID", nullable = false)
    @ManyToOne(fetch = FetchType.LAZY)
    private Area area;

    @JoinColumn(name = "AREA_TYPE_CODE", nullable = false)
    @ManyToOne(fetch = FetchType.LAZY)
    private AreaTypes areaType;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Area getArea() {
        return area;
    }

    public void setArea(Area area) {
        this.area = area;
    }

    public AreaTypes getAreaType() {
        return areaType;
    }

    public void setAreaType(AreaTypes areaType) {
        this.areaType = areaType;
    }
}
