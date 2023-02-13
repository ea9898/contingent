package moscow.ptnl.contingent.nsi.domain.area;

import moscow.ptnl.contingent.nsi.domain.NsiExternalEntity;
import moscow.ptnl.contingent.nsi.domain.NsiTablesEnum;
import moscow.ptnl.contingent.nsi.domain.annotation.MapToNsi;

import jakarta.persistence.*;
import jakarta.validation.constraints.Size;
import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.Objects;

@Entity
@Table(name = "POSITION_SUPP")
@Cacheable
@MapToNsi(table = NsiTablesEnum.POSITION_SUPP)
public class PositionSupp implements Serializable, NsiExternalEntity {

    private static final long serialVersionUID = -1047920444396677746L;

    @Id
    @Column(name = "GLOBAL_ID")
    @MapToNsi("GLOBAL_ID")
    private Long globalId;

    @Column(name = "CODE", unique = true, nullable = false)
    @MapToNsi("CODE")
    private String code;

    @Size(max = 100)
    @Column(name = "TITLE_SHORT", unique = true, nullable = false)
    @MapToNsi("TITLE_SHORT")
    private String titleShort;

//    @Column(name = "ARCHIVED", nullable = false)
//    @Convert(converter = BooleanStrictIntegerConverter.class)
//    @MapToNsi
//    private Boolean archived;

    @Column(name = "PARENT_ID")
    @MapToNsi("PARENT_ID")
    private Long parentId;

    @Column(name = "UPDATE_DATE", nullable = false)
    private LocalDateTime updateDate;

    @Column(name = "SOURCE")
    @Size(max = 4000)
    private String source;

    public String getCode() {
        return code;
    }

    public void setCode(String code) {
        this.code = code;
    }

    @Override
    public Long getGlobalId() {
        return globalId;
    }

    public void setGlobalId(Long globalId) {
        this.globalId = globalId;
    }

    public String getTitleShort() { return titleShort; }

    public void setTitleShort(String titleShort) { this.titleShort = titleShort; }

    public Long getParentId() { return parentId; }

    public void setParentId(Long parentId) {
        this.parentId = parentId;
    }

    @Override
    public Boolean getArchived() {
        return false;
    }

    @Override
    public void setArchived(Boolean archived) { }

    @Override
    public LocalDateTime getUpdateDate() {
        return updateDate;
    }

    @Override
    public void setUpdateDate(LocalDateTime updateDate) {
        this.updateDate = updateDate;
    }

    @Override
    public String getSource() {
        return source;
    }

    @Override
    public void setSource(String source) {
        this.source = source;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        PositionSupp that = (PositionSupp) o;
        return Objects.equals(globalId, that.globalId);
    }

    @Override
    public int hashCode() {
        return Objects.hash(globalId);
    }

    @Override
    public Serializable getKey() {
        return getGlobalId();
    }

}
