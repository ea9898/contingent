package moscow.ptnl.contingent.nsi.domain.area;

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

import moscow.ptnl.contingent.domain.converter.BooleanStrictIntegerConverter;
import moscow.ptnl.contingent.nsi.domain.NsiExternalEntity;
import moscow.ptnl.contingent.nsi.domain.NsiTablesEnum;
import moscow.ptnl.contingent.nsi.domain.annotation.MapToNsi;

@Entity
@Table(name = "AREA_TYPE_RELATIONS")
@Cacheable
@MapToNsi(table = NsiTablesEnum.AREA_TYPE_RELATIONS)
public class AreaTypeRelations implements Serializable, NsiExternalEntity {

    private static final long serialVersionUID = -8346464667577347303L;

    @Id
    @Column(name = "GLOBAL_ID")
    @MapToNsi("GLOBAL_ID")
    private Long globalId;

    @JoinColumn(name = "DEPENDENT_AREA_TYPE_CODE")
    @ManyToOne(fetch = FetchType.LAZY)
    @MapToNsi(value="DEPENDENT_AREA_TYPE_CODE", findEntityByField = "globalId")
    private AreaType dependentAreaType;

    @JoinColumn(name = "PRIMARY_AREA_TYPE_CODE")
    @ManyToOne(fetch = FetchType.LAZY)
    @MapToNsi(value="PRIMARY_AREA_TYPE_CODE", findEntityByField = "globalId")
    private AreaType primaryAreaType;

    @Column(name = "ARCHIVED")
    @Convert(converter = BooleanStrictIntegerConverter.class)
    @MapToNsi
    private Boolean archived;

    @Column(name = "UPDATE_DATE", nullable = false)
    private LocalDateTime updateDate;

    @Column(name = "SOURCE")
    @Size(max = 4000)
    private String source;

    @Override
    public Serializable getKey() {
        return getGlobalId();
    }

    @Override
    public Long getGlobalId() {
        return globalId;
    }

    public void setGlobalId(Long globalId) {
        this.globalId = globalId;
    }

    @Override
    public Boolean getArchived() {
        return archived;
    }

    @Override
    public void setArchived(Boolean archived) {
        this.archived = archived;
    }

    public AreaType getDependentAreaType() {
        return dependentAreaType;
    }

    public void setDependentAreaType(AreaType dependentAreaType) {
        this.dependentAreaType = dependentAreaType;
    }

    public AreaType getPrimaryAreaType() {
        return primaryAreaType;
    }

    public void setPrimaryAreaType(AreaType primaryAreaType) {
        this.primaryAreaType = primaryAreaType;
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
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj instanceof AreaTypeRelations) {
            return ((AreaTypeRelations) obj).getGlobalId().equals(this.getGlobalId());
        }
        return false;
    }

    @Override
    public int hashCode() {        
        return this.getGlobalId().hashCode();
    }
}
