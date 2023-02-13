package moscow.ptnl.contingent.domain.area.entity;


import jakarta.persistence.Cacheable;
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
import moscow.ptnl.contingent.nsi.domain.area.PolicyType;
import org.hibernate.annotations.Proxy;

@Entity
@Table(name = "AREA_POLICY_TYPES")
@Proxy(lazy=false)
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

    @JoinColumn(name = "POLICY_TYPE_CODE", nullable = false)
    @ManyToOne(fetch = FetchType.LAZY)
    private PolicyType policyType;

    public AreaPolicyTypes() {
    }

    public AreaPolicyTypes(Area area, PolicyType policyType) {
        this.area = area;
        this.policyType = policyType;
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

    public PolicyType getPolicyType() {
        return policyType;
    }

    public void setPolicyType(PolicyType policyType) {
        this.policyType = policyType;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        AreaPolicyTypes that = (AreaPolicyTypes) o;
        return Objects.equals(id, that.id) &&
                Objects.equals(area, that.area) &&
                Objects.equals(policyType, that.policyType);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id, area, policyType);
    }
}
