package moscow.ptnl.contingent.area.entity.nsi;

import moscow.ptnl.contingent.area.entity.area.Area;

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
@Table(name = "AREA_POLICY_TYPES")
@SequenceGenerator(name = "seq_area_policy_types", sequenceName = "seq_area_policy_types", allocationSize=1)
@Cacheable
public class AreaPolicyTypes implements Serializable {

    private static final long serialVersionUID = -1047920444396677745L;

    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator="seq_area_policy_types")
    @Column(name = "id", unique = true, nullable = false)
    private Long id;

    @JoinColumn(name = "AREA_ID")
    @ManyToOne(fetch = FetchType.LAZY)
    private Area area;

    @Column(name = "POLICY_TYPE_CODE", nullable = false)
    private Long policyTypeCode;

    public AreaPolicyTypes() {
    }

    public AreaPolicyTypes(Area area, Long policyTypeCode) {
        this.area = area;
        this.policyTypeCode = policyTypeCode;
    }

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

    public Long getPolicyTypeCode() {
        return policyTypeCode;
    }

    public void setPolicyTypeCode(Long policyTypeCode) {
        this.policyTypeCode = policyTypeCode;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        AreaPolicyTypes that = (AreaPolicyTypes) o;
        return Objects.equals(id, that.id) &&
                Objects.equals(area, that.area) &&
                Objects.equals(policyTypeCode, that.policyTypeCode);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id, area, policyTypeCode);
    }
}
