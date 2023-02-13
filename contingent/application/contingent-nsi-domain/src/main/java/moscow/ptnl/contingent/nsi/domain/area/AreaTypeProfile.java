package moscow.ptnl.contingent.nsi.domain.area;

import moscow.ptnl.contingent.domain.converter.BooleanStrictIntegerConverter;
import moscow.ptnl.contingent.nsi.domain.NsiExternalEntity;
import moscow.ptnl.contingent.nsi.domain.NsiTablesEnum;
import moscow.ptnl.contingent.nsi.domain.annotation.MapToNsi;
import org.hibernate.annotations.Proxy;

import jakarta.persistence.Cacheable;
import jakarta.persistence.Column;
import jakarta.persistence.Convert;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;
import jakarta.validation.constraints.Size;
import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.Objects;

@Entity
@Table(name = "AREA_TYPE_PROFILE")
@Cacheable
@Proxy(lazy = false)
@MapToNsi(table = NsiTablesEnum.AREA_TYPE_PROFILE)
public class AreaTypeProfile extends CodeName implements Serializable, NsiExternalEntity {

    private static final long serialVersionUID = 1281003603333730792L;

    @Id
    @Column(name = "CODE", unique = true, nullable = false)
    @MapToNsi
    private Long code;

    @Column(name = "TITLE", nullable = false)
    @MapToNsi
    private String title;

    @Column(name = "GLOBAL_ID", nullable = false)
    @MapToNsi("GLOBAL_ID")
    private Long globalId;

    @JoinColumn(name = "AREA_TYPE_CODE", nullable = false)
    @ManyToOne(fetch = FetchType.LAZY)
    @MapToNsi(value = "AREA_TYPE_CODE", findEntityByField = "globalId")
    private AreaType areaType;

    @Column(name = "ARCHIVED", nullable = false)
    @Convert(converter = BooleanStrictIntegerConverter.class)
    @MapToNsi
    private Boolean archived;

    @Column(name = "UPDATE_DATE", nullable = false)
    private LocalDateTime updateDate;

    @Column(name = "SOURCE")
    @Size(max = 4000)
    private String source;

    @Override
    public Long getCode() {
        return code;
    }

    @Override
    public void setCode(Long code) {
        this.code = code;
    }

    @Override
    public String getTitle() {
        return title;
    }

    @Override
    public void setTitle(String title) {
        this.title = title;
    }

    @Override
    public Long getGlobalId() {
        return globalId;
    }

    public void setGlobalId(Long globalId) {
        this.globalId = globalId;
    }

    public AreaType getAreaType() {
        return areaType;
    }

    public void setAreaType(AreaType areaType) {
        this.areaType = areaType;
    }

    @Override
    public LocalDateTime getUpdateDate() { return updateDate; }

    @Override
    public String getSource() { return source; }

    @Override
    public void setUpdateDate(LocalDateTime updateDate) { this.updateDate = updateDate; }

    @Override
    public void setSource(String source) { this.source = source; }

    @Override
    public Boolean getArchived() {
        return archived;
    }

    @Override
    public void setArchived(Boolean archived) {
        this.archived = archived;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        AreaTypeProfile that = (AreaTypeProfile) o;
        return Objects.equals(code, that.code) &&
                Objects.equals(title, that.title) &&
                Objects.equals(globalId, that.globalId) &&
                Objects.equals(areaType, that.areaType) &&
                Objects.equals(archived, that.archived);
    }

    @Override
    public int hashCode() {
        return Objects.hash(code, title, globalId, areaType, archived);
    }

    @Override
    public Serializable getKey() {
        return getCode();
    }
}
