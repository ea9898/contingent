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
@Table(name = "MAPPING_POSITIONCODE_TO_OTHERPOSITION")
@Cacheable
@MapToNsi(table = NsiTablesEnum.MAPPING_POSITIONCODE_TO_OTHERPOSITION)
public class MappingPositionCodeToOtherPosition implements Serializable, NsiExternalEntity {

    private static final long serialVersionUID = -1047920444396677746L;

    @Id
    @Column(name = "GLOBAL_ID")
    @MapToNsi("GLOBAL_ID")
    private Long globalId;

    @Column(name = "POSITIONCODE_ID", nullable = false)
    @MapToNsi("POSITIONCODE_ID")
    private Long positionCodeId;

    @Column(name = "EGISZWORKPOSITION_ID")
    @MapToNsi("EGISZWORKPOSITION_ID")
    private Long egiszworkpositionId;

    @Column(name = "PS_GLOBAL_ID")
    @MapToNsi("PS_GLOBAL_ID")
    private Long psGlobalId;

//    @Column(name = "ARCHIVED", nullable = false)
//    @Convert(converter = BooleanStrictIntegerConverter.class)
//    @MapToNsi
//    private Boolean archived;

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

    public Long getPositionCodeId() { return positionCodeId; }

    public void setPositionCodeId(Long positionCodeId) { this.positionCodeId = positionCodeId; }

    public Long getEgiszworkpositionId() { return egiszworkpositionId; }

    public void setEgiszworkpositionId(Long egiszworkpositionId) { this.egiszworkpositionId = egiszworkpositionId; }

    public Long getPsGlobalId() { return psGlobalId; }

    public void setPsGlobalId(Long psGlobalId) { this.psGlobalId = psGlobalId; }

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
        MappingPositionCodeToOtherPosition that = (MappingPositionCodeToOtherPosition) o;
        return Objects.equals(globalId, that.globalId);
    }

    @Override
    public int hashCode() {
        return Objects.hash(globalId);
    }

    @Override
    public Serializable getKey() {
        return globalId;
    }

}
