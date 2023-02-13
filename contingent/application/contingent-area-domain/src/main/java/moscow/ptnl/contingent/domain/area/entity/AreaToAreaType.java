package moscow.ptnl.contingent.domain.area.entity;

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
import moscow.ptnl.contingent.nsi.domain.area.AreaType;
import org.hibernate.annotations.Proxy;

@Entity
@Table(name = "AREA_TO_AREA_TYPE")
@Proxy(lazy=false)
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
