package moscow.ptnl.contingent.area.entity.nsi;

import moscow.ptnl.contingent.area.entity.converter.BooleanStrictIntegerConverter;
import moscow.ptnl.contingent.domain.nsi.NsiTablesEnum;
import moscow.ptnl.contingent.domain.nsi.annotation.MapToNsi;

import javax.persistence.Cacheable;
import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.validation.constraints.Size;
import java.io.Serializable;
import java.util.Objects;

@Entity
@Table(name = "SPECIALIZATION")
@Cacheable
@MapToNsi(table = NsiTablesEnum.SPECIALIZATIONS)
public class Specialization implements Serializable {

    private static final long serialVersionUID = -3935499028862334002L;

    @Id
    @Column(name = "GLOBAL_ID", unique = true, nullable = false)
    @MapToNsi("GLOBAL_ID")
    private Long globalId;

    @Size(max = 100)
    @Column(name = "TITLE", nullable = false)
    @MapToNsi
    private String title;

    @Column(name = "CODE")
    @MapToNsi
    private Long code;

    @Column(name = "ARCHIVED", nullable = false)
    @Convert(converter = BooleanStrictIntegerConverter.class)
    @MapToNsi
    private Boolean archived;

    public Specialization() {
    }

    public Specialization(Long id, String title, Boolean archived) {
        this.globalId = id;
        this.title = title;
        this.archived = archived;
    }

    public Long getGlobalId() {
        return globalId;
    }

    public void setGlobalId(Long globalId) {
        this.globalId = globalId;
    }

    public Long getCode() {
        return code;
    }

    public void setCode(Long code) {
        this.code = code;
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public Boolean getArchived() { return archived; }

    public void setArchived(Boolean archived) { this.archived = archived; }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj != null && obj instanceof Specialization) {
            return ((Specialization) obj).getGlobalId().equals(this.globalId);
        }
        return false;
    }

    @Override
    public int hashCode() {        
        return Objects.hashCode(this.globalId);
    }
}
