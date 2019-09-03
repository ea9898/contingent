package moscow.ptnl.contingent.area.entity.area;

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
import moscow.ptnl.contingent.nsi.domain.area.AreaType;

@Entity
@Table(name = "AREA_TO_AREA_TYPE")
@SequenceGenerator(name = "SEQ_AREA_TO_AREA_TYPE", sequenceName = "SEQ_AREA_TO_AREA_TYPE", allocationSize=1)
public class AreaToAreaType implements Serializable {

    private static final long serialVersionUID = 3586944486563837581L;

    @Id
    @GeneratedValue(strategy= GenerationType.SEQUENCE, generator="SEQ_AREA_TO_AREA_TYPE")
    @Column(name = "ID", unique = true, nullable = false)
    private Long id;

    @JoinColumn(name = "AREA_ID", nullable = false)
    @ManyToOne(fetch = FetchType.LAZY)
    private Area area;

    @JoinColumn(name = "AREA_TYPE_CODE", nullable = false)
    @ManyToOne(fetch = FetchType.LAZY)
    private AreaType areaType;

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

    public AreaType getAreaType() { return areaType; }

    public void setAreaType(AreaType areaType) { this.areaType = areaType; }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj != null && obj instanceof AreaToAreaType) {
            return ((AreaToAreaType) obj).getId().equals(this.id);
        }
        return false;
    }

    @Override
    public int hashCode() {        
        return Objects.hashCode(this.id);
    }
}
