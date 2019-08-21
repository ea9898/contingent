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
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import java.io.Serializable;
import java.util.Objects;
import moscow.ptnl.contingent.domain.Keyable;
import moscow.ptnl.contingent.domain.nsi.NsiTablesEnum;
import moscow.ptnl.contingent.domain.nsi.annotation.MapToNsi;

@Entity
@SequenceGenerator(name = "SEQ_AREA_TYPE_SPECIALIZATIONS", sequenceName = "SEQ_AREA_TYPE_SPECIALIZATIONS", allocationSize=1)
@Table(name = "AREA_TYPE_SPECIALIZATIONS")
@Cacheable
@MapToNsi(table = NsiTablesEnum.AREA_TYPE_SPECIALIZATIONS)
public class AreaTypeSpecializations implements Serializable, Keyable {

    private static final long serialVersionUID = -786212543217911093L;

    @Id
    @GeneratedValue(strategy= GenerationType.SEQUENCE, generator="SEQ_AREA_TYPE_SPECIALIZATIONS")
    @Column(name = "ID", unique = true, nullable = false)
    @MapToNsi
    private Long id;

    @JoinColumn(name = "AREA_TYPE_CODE", nullable = false)
    @ManyToOne(fetch = FetchType.LAZY)
    @MapToNsi("AREA_TYPE_CODE")
    private AreaType areaType;


    @Column(name = "SPECIALIZATION_CODE")
    @MapToNsi("SPECIALIZATION_CODE")
    private Long specializationCode;

    @Column(name = "ARCHIVED")
    @Convert(converter = BooleanStrictIntegerConverter.class)
    @MapToNsi
    private Boolean archived;

    @Column(name = "GLOBAL_ID")
    @MapToNsi("GLOBAL_ID")
    private Long globalId;

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

    @Override
    public Serializable getKey() {
        return getId();
    }
}
