package moscow.ptnl.contingent.area.entity.nsi;

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

import moscow.ptnl.contingent.area.entity.converter.BooleanStrictIntegerConverter;
import moscow.ptnl.contingent.domain.Keyable;

@Entity
@Table(name = "AREA_TYPE_RELATIONS")
@Cacheable
public class AreaTypeRelations implements Serializable, Keyable {

    private static final long serialVersionUID = -8346464667577347303L;

    @Id
    @Column(name = "ID")
    private Long id;

    @JoinColumn(name = "DEPENDENT_AREA_TYPE_CODE")
    @ManyToOne(fetch = FetchType.LAZY)
    private AreaType dependentAreaType;

    @JoinColumn(name = "PRIMARY_AREA_TYPE_CODE")
    @ManyToOne(fetch = FetchType.LAZY)
    private AreaType primaryAreaType;

    @Column(name = "ARCHIVED")
    @Convert(converter = BooleanStrictIntegerConverter.class)
    private Boolean archived;

    @Column(name = "GLOBAL_ID")
    private Long globalId;

    @Override
    public Serializable getKey() {
        return getId();
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
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
            return ((AreaTypeRelations) obj).getId().equals(this.getId());
        }
        return false;
    }

    @Override
    public int hashCode() {        
        return this.getId().hashCode();
    }

    public enum FieldsEnum {
        ID,
        DEPENDENT_AREA_TYPE_CODE,
        PRIMARY_AREA_TYPE_CODE,
        ARCHIVED
    }
}
