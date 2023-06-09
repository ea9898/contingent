package moscow.ptnl.contingent.nsi.domain.area;

import jakarta.persistence.Cacheable;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.SequenceGenerator;
import jakarta.persistence.Table;
import jakarta.validation.constraints.Size;
import java.io.Serializable;
import java.util.Objects;

@Entity
@Table(name = "NSI_BUILDING_REGISTRY")
@SequenceGenerator(name = "SEQ_BUILDING_REGISTRY", sequenceName = "SEQ_BUILDING_REGISTRY", allocationSize=1)
@Cacheable
public class NsiBuildingRegistry implements Serializable {

    private static final long serialVersionUID = 5017009667346896559L;

    @Id
    @GeneratedValue(strategy= GenerationType.SEQUENCE, generator="SEQ_BUILDING_REGISTRY")
    @Column(name = "ID", unique = true, nullable = false)
    private Long id;

    @Column(name = "GLOBAL_ID")
    private Long globalId;

    @Column(name = "ADDR_ID")
    private Long addrId;

    @JoinColumn(name = "AFE_ID", nullable = false)
    @ManyToOne(fetch = FetchType.LAZY)
    private NsiAddressFormingElement addressFormingElement;

    @Size(max = 256)
    @Column(name = "L1_TYPE")
    private String l1Type;

    @Size(max = 256)
    @Column(name = "L1_VALUE")
    private String l1Value;

    @Size(max = 256)
    @Column(name = "L2_TYPE")
    private String l2Type;

    @Size(max = 256)
    @Column(name = "L2_VALUE")
    private String l2Value;

    @Size(max = 256)
    @Column(name = "L3_TYPE")
    private String l3Type;

    @Size(max = 256)
    @Column(name = "L3_VALUE")
    private String l3Value;

    @Size(max = 4000)
    @Column(name = "ADDRESS")
    private String address;

    public NsiBuildingRegistry() {
    }

    public NsiBuildingRegistry(Long addrId, NsiAddressFormingElement addressFormingElement, String l1Type, String l1Value, String l2Type, String l2Value, String l3Type, String l3Value) {
        this.addrId = addrId;
        this.addressFormingElement = addressFormingElement;
        this.l1Type = l1Type;
        this.l1Value = l1Value;
        this.l2Type = l2Type;
        this.l2Value = l2Value;
        this.l3Type = l3Type;
        this.l3Value = l3Value;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getGlobalId() {
        return globalId;
    }

    public void setGlobalId(Long globalId) {
        this.globalId = globalId;
    }

    public Long getAddrId() {
        return addrId;
    }

    public void setAddrId(Long addrId) {
        this.addrId = addrId;
    }

    public NsiAddressFormingElement getAddressFormingElement() {
        return addressFormingElement;
    }

    public void setAddressFormingElement(NsiAddressFormingElement addressFormingElement) {
        this.addressFormingElement = addressFormingElement;
    }

    public String getL1Type() {
        return l1Type;
    }

    public void setL1Type(String l1Type) {
        this.l1Type = l1Type;
    }

    public String getL1Value() {
        return l1Value;
    }

    public void setL1Value(String l1Value) {
        this.l1Value = l1Value;
    }

    public String getL2Type() {
        return l2Type;
    }

    public void setL2Type(String l2Type) {
        this.l2Type = l2Type;
    }

    public String getL2Value() {
        return l2Value;
    }

    public void setL2Value(String l2Value) {
        this.l2Value = l2Value;
    }

    public String getL3Type() {
        return l3Type;
    }

    public void setL3Type(String l3Type) {
        this.l3Type = l3Type;
    }

    public String getL3Value() {
        return l3Value;
    }

    public void setL3Value(String l3Value) {
        this.l3Value = l3Value;
    }

    public String getAddress() {
        return address;
    }

    public void setAddress(String address) {
        this.address = address;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj != null && obj instanceof NsiBuildingRegistry) {
            return ((NsiBuildingRegistry) obj).getId().equals(this.id);
        }
        return false;
    }

    @Override
    public int hashCode() {        
        return Objects.hashCode(this.id);
    }
}
