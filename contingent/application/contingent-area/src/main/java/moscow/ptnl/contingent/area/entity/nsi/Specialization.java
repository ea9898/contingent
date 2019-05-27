package moscow.ptnl.contingent.area.entity.nsi;

import moscow.ptnl.contingent.area.entity.converter.BooleanStrictIntegerConverter;

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
//@Cacheable
public class Specialization implements Serializable {

    private static final long serialVersionUID = -3935499028862334002L;

    @Id
    @Column(name = "ID", unique = true, nullable = false)
    private Long id;

    @Size(max = 100)
    @Column(name = "TITLE", nullable = false)
    private String title;

    @Column(name = "ARCHIVE", nullable = false)
    @Convert(converter = BooleanStrictIntegerConverter.class)
    private Boolean archive;

    public Specialization() {
    }

    public Specialization(Long id, String title, Boolean archive) {
        this.id = id;
        this.title = title;
        this.archive = archive;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public Boolean getArchive() { return archive; }

    public void setArchive(Boolean archive) { this.archive = archive; }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj != null && obj instanceof Specialization) {
            return ((Specialization) obj).getId().equals(this.id);
        }
        return false;
    }

    @Override
    public int hashCode() {        
        return Objects.hashCode(this.id);
    }
}
