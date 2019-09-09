package moscow.ptnl.contingent.nsi.domain.area;

import javax.persistence.Cacheable;
import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.validation.constraints.Size;
import java.io.Serializable;
import java.util.Objects;
import moscow.ptnl.contingent.domain.converter.BooleanStrictIntegerConverter;
import moscow.ptnl.contingent.nsi.domain.NsiTablesEnum;
import moscow.ptnl.contingent.nsi.domain.annotation.MapToNsi;

@Entity
@Table(name = "POLICY_TYPE")
@Cacheable
@MapToNsi(table = NsiTablesEnum.POLICY_TYPE)
public class PolicyType implements Serializable {

    private static final long serialVersionUID = -1047920444396677745L;

    @Id
    @Column(name = "CODE", unique = true, nullable = false)
    @MapToNsi
    private Long code;

    @Size(max = 100)
    @Column(name = "TITLE")
    @MapToNsi("TITLE")
    private String title;

    @Column(name = "ARCHIVED", nullable = false)
    @Convert(converter = BooleanStrictIntegerConverter.class)
    @MapToNsi
    private Boolean archived;

    @Column(name = "GLOBAL_ID")
    @MapToNsi("GLOBAL_ID")
    private Long globalId;

    public Long getCode() {
        return code;
    }

    public void setCode(Long code) {
        this.code = code;
    }

    public Long getGlobalId() {
        return globalId;
    }

    public void setGlobalId(Long globalId) {
        this.globalId = globalId;
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
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
        PolicyType that = (PolicyType) o;
        return Objects.equals(code, that.code);
    }

    @Override
    public int hashCode() {
        return Objects.hash(code);
    }
}
