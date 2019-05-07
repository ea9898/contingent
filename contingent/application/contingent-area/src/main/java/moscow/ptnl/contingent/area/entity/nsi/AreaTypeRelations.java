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

@Entity
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
}
