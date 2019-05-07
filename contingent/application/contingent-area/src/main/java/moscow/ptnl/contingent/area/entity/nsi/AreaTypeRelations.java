package moscow.ptnl.contingent.area.entity.nsi;

import moscow.ptnl.contingent.area.entity.area.MuProfile;

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
    @JoinColumn(name = "DEPENDENT_MU_PROFILE_ID")
    @ManyToOne(fetch = FetchType.LAZY)
    private MuProfile dependentMuProfile;

    @Id
    @JoinColumn(name = "PRIMARY_MU_PROFILE_ID")
    @ManyToOne(fetch = FetchType.LAZY)
    private MuProfile primaryMuProfile;
    
    public Key getKey() {
        return new Key(dependentMuProfile, primaryMuProfile);
    }

    public MuProfile getDependentMuProfile() {
        return dependentMuProfile;
    }

    public void setDependentMuProfile(MuProfile dependentMuProfile) {
        this.dependentMuProfile = dependentMuProfile;
    }

    public MuProfile getPrimaryMuProfile() {
        return primaryMuProfile;
    }

    public void setPrimaryMuProfile(MuProfile primaryMuProfile) {
        this.primaryMuProfile = primaryMuProfile;
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
        
        private MuProfile dependentMuProfile;
        private MuProfile primaryMuProfile;
        
        public Key(){}
        
        public Key(MuProfile dependentMuProfile, MuProfile primaryMuProfile) {
            this.dependentMuProfile = dependentMuProfile;
            this.primaryMuProfile = primaryMuProfile;
        }
        

        public MuProfile getDependentMuProfile() {
            return dependentMuProfile;
        }

        public void setDependentMuProfile(MuProfile dependentMuProfile) {
            this.dependentMuProfile = dependentMuProfile;
        }

        public MuProfile getPrimaryMuProfile() {
            return primaryMuProfile;
        }

        public void setPrimaryMuProfile(MuProfile primaryMuProfile) {
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
