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

@Entity
@Table(name = "Gender")
@Cacheable
@MapToNsi(table = NsiTablesEnum.GENDER)
public class Gender implements Serializable {

    private static final long serialVersionUID = 7174737671670446575L;

    @Id
    @Column(name = "ID", unique = true, nullable = false)
    @MapToNsi
    private Long id;

    @Size(max = 50)
    @Column(name = "CODE", nullable = false)
    @MapToNsi
    private String code;

    @Size(max = 50)
    @Column(name = "TITLE")
    @MapToNsi
    private String title;

    @Column(name = "ARCHIVED", nullable = false)
    @Convert(converter = BooleanStrictIntegerConverter.class)
    @MapToNsi
    private Boolean archived;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getCode() {
        return code;
    }

    public void setCode(String code) {
        this.code = code;
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
        if (!(o instanceof Gender)) return false;

        Gender gender = (Gender) o;

        if (!id.equals(gender.id)) return false;
        if (code != null ? !code.equals(gender.code) : gender.code != null) return false;
        if (title != null ? !title.equals(gender.title) : gender.title != null) return false;
        return archived != null ? archived.equals(gender.archived) : gender.archived == null;
    }

    @Override
    public int hashCode() {
        int result = id.hashCode();
        result = 31 * result + (code != null ? code.hashCode() : 0);
        result = 31 * result + (title != null ? title.hashCode() : 0);
        result = 31 * result + (archived != null ? archived.hashCode() : 0);
        return result;
    }
}
