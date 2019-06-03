package moscow.ptnl.contingent.area.entity.nsi;

import javax.persistence.Cacheable;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import java.io.Serializable;
import java.util.Objects;
import javax.persistence.IdClass;
import moscow.ptnl.contingent.area.entity.nsi.AreaType;

@Entity
@IdClass(AreaTypeRelations.Key.class)
@Table(name = "AREA_TYPE_RELATIONS")
@Cacheable
public class AreaTypeRelations implements Serializable {

    private static final long serialVersionUID = -8346464667577347303L;

    @Id
    @JoinColumn(name = "DEPENDENT_AREA_TYPE_ID")
    @ManyToOne(fetch = FetchType.LAZY)
    private AreaType dependentAreaType;

    @Id
    @JoinColumn(name = "PRIMARY_AREA_TYPE_ID")
    @ManyToOne(fetch = FetchType.LAZY)
    private AreaType primaryAreaType;
    
    public Key getKey() {
        return new Key(dependentAreaType, primaryAreaType);
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
        if (obj != null && obj instanceof AreaTypeRelations) {
            return ((AreaTypeRelations) obj).getKey().equals(this.getKey());
        }
        return false;
    }

    @Override
    public int hashCode() {        
        return this.getKey().hashCode();
    }
    
    /**
     * Композитный ключ.
     */
    public static class Key implements Serializable {
        
        private AreaType dependentAreaType;
        private AreaType primaryAreaType;
        
        public Key() {}
        
        public Key(AreaType dependentAreaType, AreaType primaryAreaType) {
            this.dependentAreaType = dependentAreaType;
            this.primaryAreaType = primaryAreaType;
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
            if(this == obj)
                return true;
            if (obj != null && obj instanceof Key) {
                Key other = (Key) obj;
                if (other.getDependentAreaType() == null || other.getPrimaryAreaType() == null)
                    return false;
                return other.getDependentAreaType().equals(this.getDependentAreaType()) && other.getPrimaryAreaType().equals(this.getPrimaryAreaType());
            }
            return false;
        }

        @Override
        public int hashCode() {
            return Objects.hash(this.getDependentAreaType(), this.getPrimaryAreaType());
        }
        
    }
}