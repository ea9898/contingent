package moscow.ptnl.contingent.area.entity.nsi;

import moscow.ptnl.contingent.area.entity.converter.BooleanStrictIntegerConverter;

import javax.persistence.Cacheable;
import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import java.io.Serializable;
import java.util.Objects;

@Entity
@Table(name = "POLICY_SERVICE_TYPES")
@Cacheable
public class PolicyServiceTypes implements Serializable {

    private static final long serialVersionUID = -8178766051622678085L;

    @Id
    @Column(name = "ID", unique = true, nullable = false)
    private Long id;

    @JoinColumn(name = "AREA_TYPE_CODE", nullable = false)
    @ManyToOne(fetch = FetchType.LAZY)
    private AreaType areaType;

    @JoinColumn(name = "POLICY_TYPE_CODE", nullable = false)
    @ManyToOne(fetch = FetchType.LAZY)
    private PolicyType policyType;

    @Column(name = "ARCHIVED", nullable = false)
    @Convert(converter = BooleanStrictIntegerConverter.class)
    private Boolean archived;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public AreaType getAreaType() {
        return areaType;
    }

    public void setAreaType(AreaType areaType) {
        this.areaType = areaType;
    }

    public PolicyType getPolicyType() {
        return policyType;
    }

    public void setPolicyType(PolicyType policyType) {
        this.policyType = policyType;
    }

    public Boolean getArchived() {
        return archived;
    }

    public void setArchived(Boolean archived) {
        this.archived = archived;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        PolicyServiceTypes that = (PolicyServiceTypes) o;
        return Objects.equals(id, that.id) &&
                Objects.equals(areaType, that.areaType) &&
                Objects.equals(policyType, that.policyType) &&
                Objects.equals(archived, that.archived);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id, areaType, policyType, archived);
    }
}
