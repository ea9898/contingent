package moscow.ptnl.contingent.area.entity.nsi;

import javax.persistence.Cacheable;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.validation.constraints.Size;
import java.io.Serializable;

@Entity
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

    public AreaTypes getDependentAreaType() {
        return dependentAreaType;
    }

    public void setDependentAreaType(AreaTypes dependentAreaType) {
        this.dependentAreaType = dependentAreaType;
    }

    public AreaTypes getPrimaryAreaType() {
        return primaryAreaType;
    }

    public void setPrimaryAreaType(AreaTypes primaryAreaType) {
        this.primaryAreaType = primaryAreaType;
    }
}
