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

@Entity
@IdClass(AreaTypeRelations.Key.class)
@Table(name = "AREA_TYPE_RELATIONS")
@Cacheable
public class AreaTypeRelations implements Serializable {

    private static final long serialVersionUID = -8346464667577347303L;

    @Id
    @JoinColumn(name = "DEPENDENT_AREA_TYPE_ID")
    @ManyToOne(fetch = FetchType.LAZY)
    private AreaTypes dependentAreaType;

    @Id
    @JoinColumn(name = "PRIMARY_AREA_TYPE_ID")
    @ManyToOne(fetch = FetchType.LAZY)
    private AreaTypes primaryAreaType;
    
    public Key getKey() {
        return new Key(dependentAreaType, primaryAreaType);
    }

    public AreaTypes getDependentMuProfile() {
        return dependentAreaType;
    }

    public void setDependentMuProfile(AreaTypes dependentMuProfile) {
        this.dependentAreaType = dependentMuProfile;
    }

    public AreaTypes getPrimaryMuProfile() {
        return primaryAreaType;
    }

    public void setPrimaryMuProfile(AreaTypes primaryMuProfile) {
        this.primaryAreaType = primaryMuProfile;
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
        
        private AreaTypes dependentMuProfile;
        private AreaTypes primaryMuProfile;
        
        public Key(){}
        
        public Key(AreaTypes dependentMuProfile, AreaTypes primaryMuProfile) {
            this.dependentMuProfile = dependentMuProfile;
            this.primaryMuProfile = primaryMuProfile;
        }
        

        public AreaTypes getDependentMuProfile() {
            return dependentMuProfile;
        }

        public void setDependentMuProfile(AreaTypes dependentMuProfile) {
            this.dependentMuProfile = dependentMuProfile;
        }

        public AreaTypes getPrimaryMuProfile() {
            return primaryMuProfile;
        }

        public void setPrimaryMuProfile(AreaTypes primaryMuProfile) {
            this.primaryMuProfile = primaryMuProfile;
        }        
        
        @Override
        public boolean equals(Object obj) {
            if(this == obj)
                return true;
            if (obj != null && obj instanceof Key) {
                Key other = (Key) obj;
                if (other.getDependentMuProfile() == null || other.getPrimaryMuProfile() == null)
                    return false;
                return  other.getDependentMuProfile().equals(this.getDependentMuProfile()) && other.getPrimaryMuProfile().equals(this.getPrimaryMuProfile());
            }
            return false;
        }

        @Override
        public int hashCode() {
            return Objects.hash(this.getDependentMuProfile(), this.getPrimaryMuProfile());
        }
        
    }
}
