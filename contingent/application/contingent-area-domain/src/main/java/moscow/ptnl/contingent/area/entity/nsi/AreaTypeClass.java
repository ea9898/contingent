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
import moscow.ptnl.contingent.domain.Keyable;

@Entity
@Table(name = "AREA_TYPES_CLASS")
@Cacheable
public class AreaTypeClass implements Serializable, Keyable {

    private static final long serialVersionUID = 4979698890748802824L;

    @Id
    @Column(name = "CODE", unique = true, nullable = false)
    private Long code;

    @Size(max = 255)
    @Column(name = "TITLE")
    private String title;

    @Column(name = "ARCHIVED", nullable = false)
    @Convert(converter = BooleanStrictIntegerConverter.class)
    private Boolean archived;

    @Column(name = "GLOBAL_ID")
    private Long globalId;

    public AreaTypeClass() {
    }

    public AreaTypeClass(Long code) {
        this.code = code;
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

    public Boolean getArchived() {
        return archived;
    }

    public void setArchived(Boolean archived) {
        this.archived = archived;
    }

    public Long getGlobalId() {
        return globalId;
    }

    public void setGlobalId(Long globalId) {
        this.globalId = globalId;
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(this.code);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj instanceof AreaTypeClass) {
            return ((AreaTypeClass) obj).getCode().equals(this.code);
        }
        return false;
    }

    @Override
    public Serializable getKey() {
        return getCode();
    }

    public enum FieldsEnum {
        CODE,
        TITLE,
        ARCHIVED,
        GLOBAL_ID
    }
}
