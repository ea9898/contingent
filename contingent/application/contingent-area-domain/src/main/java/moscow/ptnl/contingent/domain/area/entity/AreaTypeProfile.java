package moscow.ptnl.contingent.domain.area.entity;

import moscow.ptnl.contingent.domain.converter.BooleanStrictIntegerConverter;
import moscow.ptnl.contingent.nsi.domain.area.AreaType;

import javax.persistence.Cacheable;
import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import java.io.Serializable;
import java.util.Objects;

@Entity
@Table(name = "AREA_TYPE_PROFILE")
@Cacheable
public class AreaTypeProfile implements Serializable {

    private static final long serialVersionUID = 1281003603333730792L;

    @Id
    @Column(name = "CODE", unique = true, nullable = false)
    private Long code;

    @Column(name = "TITLE", nullable = false)
    private String title;

    @Column(name = "GLOBAL_ID", nullable = false)
    private Long globalId;

    @JoinColumn(name = "AREA_TYPE_CODE", nullable = false)
    @ManyToOne(fetch = FetchType.LAZY)
    private AreaType areaType;

    @Column(name = "ARCHIVED", nullable = false)
    @Convert(converter = BooleanStrictIntegerConverter.class)
    private Boolean archived;

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
}
