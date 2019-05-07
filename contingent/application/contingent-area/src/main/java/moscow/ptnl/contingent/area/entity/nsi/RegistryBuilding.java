package moscow.ptnl.contingent.area.entity.nsi;

import javax.persistence.Cacheable;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.validation.constraints.Size;
import java.io.Serializable;
import java.util.Objects;

@Entity
@Table(name = "REGISTRY_BUILDING")
@Cacheable
public class RegistryBuilding implements Serializable {

    private static final long serialVersionUID = 5017009667346896559L;

    @Id
    @Column(name = "GLOBAL_ID", unique = true, nullable = false)
    private Long globalId;

    @Column(name = "ADDR_ID", nullable = false)
    private Long addrId;

    @Size(max = 32)
    @Column(name = "REGION_TE_CODE")
    private String regionTeCode;

    @Size(max = 32)
    @Column(name = "AREACODE_OMK_TE")
    private String areaCodeOmkTe;

    @Size(max = 3)
    @Column(name = "AREACODE")
    private String areaCode;

    @Size(max = 3)
    @Column(name = "CITYCODE")
    private String cityCode;

    @Size(max = 3)
    @Column(name = "PLACECODE")
    private String placeCode;

    @Size(max = 4)
    @Column(name = "PLANCODE")
    private String planCode;

    @Size(max = 4)
    @Column(name = "STREETCODE")
    private String streetCode;

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

    public String getRegionTeCode() {
        return regionTeCode;
    }

    public void setRegionTeCode(String regionTeCode) {
        this.regionTeCode = regionTeCode;
    }

    public String getAreaCodeOmkTe() {
        return areaCodeOmkTe;
    }

    public void setAreaCodeOmkTe(String areaCodeOmkTe) {
        this.areaCodeOmkTe = areaCodeOmkTe;
    }

    public String getAreaCode() {
        return areaCode;
    }

    public void setAreaCode(String areaCode) {
        this.areaCode = areaCode;
    }

    public String getCityCode() {
        return cityCode;
    }

    public void setCityCode(String cityCode) {
        this.cityCode = cityCode;
    }

    public String getPlaceCode() {
        return placeCode;
    }

    public void setPlaceCode(String placeCode) {
        this.placeCode = placeCode;
    }

    public String getPlanCode() {
        return planCode;
    }

    public void setPlanCode(String planCode) {
        this.planCode = planCode;
    }

    public String getStreetCode() {
        return streetCode;
    }

    public void setStreetCode(String streetCode) {
        this.streetCode = streetCode;
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
        if (obj != null && obj instanceof RegistryBuilding) {
            return ((RegistryBuilding) obj).getGlobalId().equals(this.globalId);
        }
        return false;
    }

    @Override
    public int hashCode() {        
        return Objects.hashCode(this.globalId);
    }
}
