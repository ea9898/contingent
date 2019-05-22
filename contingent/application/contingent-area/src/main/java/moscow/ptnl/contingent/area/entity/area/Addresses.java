package moscow.ptnl.contingent.area.entity.area;

import moscow.ptnl.contingent.area.entity.nsi.AddressFormingElement;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypes;
import moscow.ptnl.contingent.area.entity.nsi.RegistryBuilding;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.validation.constraints.Size;
import java.io.Serializable;
import java.util.Objects;

@Entity
@Table(name = "ADDRESSES")
public class Addresses implements Serializable {

    private static final long serialVersionUID = -414611125417013781L;

    @Id
    @Column(name = "ID", unique = true, nullable = false)
    private Long id;

    @Column(name = "LEVEL", nullable = false)
    private Integer level;

    @JoinColumn(name = "AFE_ID")
    @ManyToOne(fetch = FetchType.LAZY)
    private AddressFormingElement addressFormingElement;

    @JoinColumn(name = "REGISTRY_BUILDING_ID")
    @ManyToOne(fetch = FetchType.LAZY)
    private RegistryBuilding registryBuilding;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Integer getLevel() {
        return level;
    }

    public void setLevel(Integer level) {
        this.level = level;
    }

    public AddressFormingElement getAddressFormingElement() {
        return addressFormingElement;
    }

    public void setAddressFormingElement(AddressFormingElement addressFormingElement) {
        this.addressFormingElement = addressFormingElement;
    }

    public RegistryBuilding getRegistryBuilding() {
        return registryBuilding;
    }

    public void setRegistryBuilding(RegistryBuilding registryBuilding) {
        this.registryBuilding = registryBuilding;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj != null && obj instanceof Addresses) {
            return ((Addresses) obj).getId().equals(this.id);
        }
        return false;
    }

    @Override
    public int hashCode() {        
        return Objects.hashCode(this.id);
    }
}
