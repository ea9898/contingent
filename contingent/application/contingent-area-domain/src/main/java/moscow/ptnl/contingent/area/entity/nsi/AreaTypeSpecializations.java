package moscow.ptnl.contingent.area.entity.nsi;

import moscow.ptnl.contingent.area.entity.converter.BooleanStrictIntegerConverter;
import moscow.ptnl.contingent.domain.history.meta.LogIt;

import javax.persistence.Cacheable;
import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.IdClass;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import java.io.Serializable;
import java.util.Objects;

@Entity
@SequenceGenerator(name = "SEQ_AREA_TYPE_SPECIALIZATIONS", sequenceName = "SEQ_AREA_TYPE_SPECIALIZATIONS", allocationSize=1)
@Table(name = "AREA_TYPE_SPECIALIZATIONS")
@Cacheable
public class AreaTypeSpecializations implements Serializable {

    private static final long serialVersionUID = -786212543217911093L;

    @Id
    @GeneratedValue(strategy= GenerationType.SEQUENCE, generator="SEQ_AREA_TYPE_SPECIALIZATIONS")
    @Column(name = "ID", unique = true, nullable = false)
    private Long id;

    @JoinColumn(name = "AREA_TYPE_CODE", nullable = false)
    @ManyToOne(fetch = FetchType.LAZY)
    private AreaType areaType;


    @Column(name = "SPECIALIZATION_CODE")
    private Long specializationCode;

    @Column(name = "ARCHIVED")
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

    public Long getSpecializationCode() {
        return specializationCode;
    }

    public void setSpecializationCode(Long specializationCode) {
        this.specializationCode = specializationCode;
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
        AreaTypeSpecializations that = (AreaTypeSpecializations) o;
        return Objects.equals(id, that.id) &&
                Objects.equals(areaType, that.areaType) &&
                Objects.equals(specializationCode, that.specializationCode) &&
                Objects.equals(archived, that.archived);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id, areaType, specializationCode, archived);
    }
}
