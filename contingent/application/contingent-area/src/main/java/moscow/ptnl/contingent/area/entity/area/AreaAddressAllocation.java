package moscow.ptnl.contingent.area.entity.area;

import moscow.ptnl.contingent.area.entity.nsi.Address;
import moscow.ptnl.contingent.area.entity.nsi.AddressFormingElement;
import moscow.ptnl.contingent.area.entity.nsi.RegistryBuilding;

import javax.persistence.Cacheable;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import java.io.Serializable;
import java.time.LocalDate;
import java.util.Objects;
import javax.persistence.IdClass;

@Entity
@IdClass(AreaAddressAllocation.Key.class)
@Table(name = "ADDRESS_ALLOCATION_TO_AREA")
@Cacheable
public class AreaAddressAllocation implements Serializable {

    private static final long serialVersionUID = 5982770527711526102L;

    @Id
    @JoinColumn(name = "DISTRIBUTION_TO_MO_ID")
    @ManyToOne(fetch = FetchType.LAZY)
    private MoAddressAllocation distributionToMo;

    @Id
    @JoinColumn(name = "AREA_ID")
    @ManyToOne(fetch = FetchType.LAZY)
    private Area area;

    @Column(name = "START_DATE")
    private LocalDate startDate;

    @Column(name = "END_DATE")
    private LocalDate endDate;

    @JoinColumn(name = "ADDRESS_ID")
    @ManyToOne(fetch = FetchType.LAZY)
    private Address address;

    @JoinColumn(name = "ADDRESS_FORMING_ELEMENT_ID")
    @ManyToOne(fetch = FetchType.LAZY)
    private AddressFormingElement addressFormingElement;

    @JoinColumn(name = "REGISTRY_BUILDING_ID")
    @ManyToOne(fetch = FetchType.LAZY)
    private RegistryBuilding registryBuilding;
    
    public Key getKey() {
        return new Key(this.distributionToMo, this.area);
    }

    public MoAddressAllocation getDistributionToMo() {
        return distributionToMo;
    }

    public void setDistributionToMo(MoAddressAllocation distributionToMo) {
        this.distributionToMo = distributionToMo;
    }

    public Area getArea() {
        return area;
    }

    public void setArea(Area area) {
        this.area = area;
    }

    public LocalDate getStartDate() {
        return startDate;
    }

    public void setStartDate(LocalDate startDate) {
        this.startDate = startDate;
    }

    public LocalDate getEndDate() {
        return endDate;
    }

    public void setEndDate(LocalDate endDate) {
        this.endDate = endDate;
    }

    public Address getAddress() {
        return address;
    }

    public void setAddress(Address address) {
        this.address = address;
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
        if (obj != null && obj instanceof AreaAddressAllocation) {
            return ((AreaAddressAllocation) obj).getKey().equals(this.getKey());
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
        
        private MoAddressAllocation distributionToMo;
        private Area area;
        
        public Key(){}
        
        public Key(MoAddressAllocation distributionToMo, Area area) {
            this.distributionToMo = distributionToMo;
            this.area = area;
        }
        
        public MoAddressAllocation getDistributionToMo() {
            return distributionToMo;
        }
        
        public void setDistributionToMo(MoAddressAllocation distributionToMo) {
            this.distributionToMo = distributionToMo;
        }
        
        public Area getArea() {
            return area;
        }
        
        public void setArea(Area area) {
            this.area = area;
        }
        
        @Override
        public boolean equals(Object obj) {
            if(this == obj)
                return true;
            if (obj != null && obj instanceof Key) {
                Key other = (Key) obj;
                if (other.getDistributionToMo() == null || other.getArea() == null)
                    return false;
                return  other.getDistributionToMo().equals(this.distributionToMo) && other.getArea().equals(this.area);
            }
            return false;
        }

        @Override
        public int hashCode() {
            return Objects.hash(this.distributionToMo, this.area);
        }
        
    }
}
