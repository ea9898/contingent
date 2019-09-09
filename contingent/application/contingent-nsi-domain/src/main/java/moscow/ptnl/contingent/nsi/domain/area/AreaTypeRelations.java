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
import java.io.Serializable;
import moscow.ptnl.contingent.domain.Keyable;
import moscow.ptnl.contingent.domain.converter.BooleanStrictIntegerConverter;
import moscow.ptnl.contingent.nsi.domain.NsiTablesEnum;
import moscow.ptnl.contingent.nsi.domain.annotation.MapToNsi;

@Entity
@Table(name = "AREA_TYPE_RELATIONS")
@Cacheable
@MapToNsi(table = NsiTablesEnum.AREA_TYPE_RELATIONS)
public class AreaTypeRelations implements Serializable, Keyable {

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

    @Override
    public Serializable getKey() {
        return getGlobalId();
    }

    public Long getGlobalId() {
        return globalId;
    }

    public void setGlobalId(Long globalId) {
        this.globalId = globalId;
    }

    public Boolean getArchived() {
        return archived;
    }

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
