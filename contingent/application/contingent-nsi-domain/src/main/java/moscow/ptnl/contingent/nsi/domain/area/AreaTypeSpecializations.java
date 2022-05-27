package moscow.ptnl.contingent.nsi.domain.area;

import javax.persistence.Cacheable;
import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.validation.constraints.Size;
import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.Objects;
import moscow.ptnl.contingent.domain.Keyable;
import moscow.ptnl.contingent.domain.converter.BooleanStrictIntegerConverter;
import moscow.ptnl.contingent.nsi.domain.NsiExternalEntity;
import moscow.ptnl.contingent.nsi.domain.NsiTablesEnum;
import moscow.ptnl.contingent.nsi.domain.annotation.MapToNsi;


@Entity
@Table(name = "AREA_TYPE_SPECIALIZATIONS")
@Cacheable
@MapToNsi(table = NsiTablesEnum.AREA_TYPE_SPECIALIZATIONS)
public class AreaTypeSpecializations implements Serializable, NsiExternalEntity {

    private static final long serialVersionUID = -786212543217911093L;

    @Id
    @Column(name = "GLOBAL_ID", unique = true, nullable = false)
    @MapToNsi("GLOBAL_ID")
    private Long globalId;

    @JoinColumn(name = "AREA_TYPE_CODE", nullable = false)
    @ManyToOne(fetch = FetchType.LAZY)
    @MapToNsi(value = "AREA_TYPE_CODE", findEntityByField = "globalId")
    private AreaType areaType;

    @Column(name = "SPECIALIZATION_CODE")
    @MapToNsi(value = "SPECIALIZATION_CODE", findEntityByField = "code", crossObject = Specialization.class, crossField = "globalId")
    private String specializationCode;

    @Column(name = "ARCHIVED")
    @Convert(converter = BooleanStrictIntegerConverter.class)
    @MapToNsi
    private Boolean archived;

    @Column(name = "UPDATE_DATE", nullable = false)
    private LocalDateTime updateDate;

    @Column(name = "SOURCE")
    @Size(max = 4000)
    private String source;

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

    public String getSpecializationCode() {
        return specializationCode;
    }

    public void setSpecializationCode(String specializationCode) {
        this.specializationCode = specializationCode;
    }

    public Boolean getArchived() {
        return archived;
    }

    public void setArchived(Boolean archived) {
        this.archived = archived;
    }

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
        AreaTypeSpecializations that = (AreaTypeSpecializations) o;
        return Objects.equals(globalId, that.globalId) &&
                Objects.equals(areaType, that.areaType) &&
                Objects.equals(specializationCode, that.specializationCode) &&
                Objects.equals(archived, that.archived);
    }

    @Override
    public int hashCode() {
        return Objects.hash(globalId, areaType, specializationCode, archived);
    }

    @Override
    public Serializable getKey() {
        return getGlobalId();
    }
}
