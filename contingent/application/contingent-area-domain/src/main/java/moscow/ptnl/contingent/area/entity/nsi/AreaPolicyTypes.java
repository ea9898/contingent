package moscow.ptnl.contingent.area.entity.nsi;

import javax.persistence.Cacheable;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import java.io.Serializable;

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

    @Column(name = "area_id")
    private Long area_id;

    @Column(name = "policy_type_code", nullable = false)
    private Long policyTypeCode;

    public AreaPolicyTypes() {
    }

    public AreaPolicyTypes(Long area_id, Long policyTypeCode) {
        this.area_id = area_id;
        this.policyTypeCode = policyTypeCode;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getArea_id() {
        return area_id;
    }

    public void setArea_id(Long area_id) {
        this.area_id = area_id;
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
        if (!(o instanceof AreaPolicyTypes)) return false;

        AreaPolicyTypes that = (AreaPolicyTypes) o;

        if (id != null ? !id.equals(that.id) : that.id != null) return false;
        if (area_id != null ? !area_id.equals(that.area_id) : that.area_id != null) return false;
        return policyTypeCode != null ? policyTypeCode.equals(that.policyTypeCode) : that.policyTypeCode == null;
    }

    @Override
    public int hashCode() {
        int result = id != null ? id.hashCode() : 0;
        result = 31 * result + (area_id != null ? area_id.hashCode() : 0);
        result = 31 * result + (policyTypeCode != null ? policyTypeCode.hashCode() : 0);
        return result;
    }
}
