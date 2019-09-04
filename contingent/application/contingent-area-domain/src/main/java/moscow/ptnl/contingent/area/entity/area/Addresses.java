package moscow.ptnl.contingent.area.entity.area;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.validation.constraints.Size;
import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.Objects;

@Entity
@Table(name = "ADDRESSES")
@SequenceGenerator(name = "SEQ_ADDRESSES", sequenceName = "SEQ_ADDRESSES", allocationSize=1)
public class Addresses implements Serializable {

    private static final long serialVersionUID = -414611125417013781L;

    @Id
    @GeneratedValue(strategy= GenerationType.SEQUENCE, generator="SEQ_ADDRESSES")
    @Column(name = "ID", unique = true, nullable = false)
    private Long id;

    @Column(name = "GLOBAL_ID")
    private Long globalId;

    @Column(name = "AOLEVEL")
    @Size(max = 2)
    private String aoLevel;

    @Size(max = 4)
    @Column(name = "REGION_TE_CODE")
    private String regionTeCode;

    @Size(max = 4000)
    @Column(name = "REGION_TE_NAME")
    private String regionTeName;

    @Size(max = 256)
    @Column(name = "REGION_TE_TYPENAME")
    private String regionTeTypeName;

    @Size(max = 256)
    @Column(name = "REGION_TE_TYPENAME_SHORT")
    private String regionTeTypeNameShort;

    @Size(max = 4000)
    @Column(name = "AREACODE_OMK_TE")
    private String areaCodeOmkTe;

    @Size(max = 4000)
    @Column(name = "AREA_TE_NAME")
    private String areaTeName;

    @Size(max = 128)
    @Column(name = "AREA_TE_TYPENAME")
    private String areaTeTypeName;

    @Size(max = 128)
    @Column(name = "AREA_TE_TYPENAME_SHORT")
    private String areaTeTypeNameShort;

    @Size(max = 3)
    @Column(name = "AREACODE")
    private String areaCode;

    @Size(max = 11)
    @Column(name = "AREA_BTI_CODE")
    private String areaBtiCode;

    @Size(max = 4000)
    @Column(name = "AREA_NAME")
    private String areaName;

    @Size(max = 128)
    @Column(name = "AREA_TYPENAME")
    private String areaTypeName;

    @Size(max = 128)
    @Column(name = "AREA_TYPENAME_SHORT")
    private String areaTypeNameShort;

    @Size(max = 3)
    @Column(name = "CITYCODE")
    private String cityCode;

    @Size(max = 11)
    @Column(name = "CITY_BTI_CODE")
    private String cityBtiCode;

    @Size(max = 4000)
    @Column(name = "CITY_NAME")
    private String cityName;

    @Size(max = 128)
    @Column(name = "CITY_TYPENAME")
    private String cityTypeName;

    @Size(max = 128)
    @Column(name = "CITY_TYPENAME_SHORT")
    private String cityTypeNameShort;

    @Size(max = 3)
    @Column(name = "PLACECODE")
    private String placeCode;

    @Size(max = 11)
    @Column(name = "PLACE_BTI_CODE")
    private String placeBtiCode;

    @Size(max = 4000)
    @Column(name = "PLACE_NAME")
    private String placeName;

    @Size(max = 128)
    @Column(name = "PLACE_TYPENAME")
    private String placeTypeName;

    @Size(max = 128)
    @Column(name = "PLACE_TYPENAME_SHORT")
    private String placeTypeNameShort;

    @Size(max = 3)
    @Column(name = "PLANCODE")
    private String planCode;

    @Size(max = 1)
    @Column(name = "PLAN_BTI_CODE")
    private String planBtiCode;

    @Size(max = 4000)
    @Column(name = "PLAN_NAME")
    private String planName;

    @Size(max = 128)
    @Column(name = "PLAN_TYPENAME")
    private String planTypeName;

    @Size(max = 128)
    @Column(name = "PLAN_TYPENAME_SHORT")
    private String planTypeNameShort;

    @Size(max = 4)
    @Column(name = "STREETCODE")
    private String streetCode;

    @Size(max = 4000)
    @Column(name = "STREET_NAME")
    private String streetName;

    @Size(max = 10)
    @Column(name = "STREET_OMK_UM")
    private String streetOmkUm;

    @Size(max = 128)
    @Column(name = "STREET_TYPENAME")
    private String streetTypeName;

    @Size(max = 128)
    @Column(name = "STREET_TYPENAME_SHORT")
    private String streetTypeNameShort;

    @Size(max = 11)
    @Column(name = "STREET_BTI_CODE")
    private String streetBtiCode;

    @Size(max = 256)
    @Column(name = "L1_TYPE")
    private String l1Type;

    @Size(max = 16)
    @Column(name = "L1_TYPE_SHORT")
    private String l1TypeShort;

    @Size(max = 256)
    @Column(name = "L1_VALUE")
    private String l1Value;

    @Size(max = 256)
    @Column(name = "L2_TYPE")
    private String l2Type;

    @Size(max = 16)
    @Column(name = "L2_TYPE_SHORT")
    private String l2TypeShort;

    @Size(max = 256)
    @Column(name = "L2_VALUE")
    private String l2Value;

    @Size(max = 256)
    @Column(name = "L3_TYPE")
    private String l3Type;

    @Size(max = 16)
    @Column(name = "L3_TYPE_SHORT")
    private String l3TypeShort;

    @Size(max = 256)
    @Column(name = "L3_VALUE")
    private String l3Value;

    @Size(max = 4000)
    @Column(name = "ADDRESS", nullable = false)
    private String address;

    @Column(name = "CREATE_DATE")
    private LocalDateTime createDate;

    @Column(name = "UPDATE_DATE", nullable = false)
    private LocalDateTime updateDate;

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

    public String getAoLevel() {
        return aoLevel;
    }

    public void setAoLevel(String aoLevel) {
        this.aoLevel = aoLevel;
    }

    public String getRegionTeCode() {
        return regionTeCode;
    }

    public void setRegionTeCode(String regionTeCode) {
        this.regionTeCode = regionTeCode;
    }

    public String getRegionTeName() {
        return regionTeName;
    }

    public void setRegionTeName(String regionTeName) {
        this.regionTeName = regionTeName;
    }

    public String getRegionTeTypeName() {
        return regionTeTypeName;
    }

    public void setRegionTeTypeName(String regionTeTypeName) {
        this.regionTeTypeName = regionTeTypeName;
    }

    public String getRegionTeTypeNameShort() {
        return regionTeTypeNameShort;
    }

    public void setRegionTeTypeNameShort(String regionTeTypeNameShort) {
        this.regionTeTypeNameShort = regionTeTypeNameShort;
    }

    public String getAreaCodeOmkTe() {
        return areaCodeOmkTe;
    }

    public void setAreaCodeOmkTe(String areaCodeOmkTe) {
        this.areaCodeOmkTe = areaCodeOmkTe;
    }

    public String getAreaTeName() {
        return areaTeName;
    }

    public void setAreaTeName(String areaTeName) {
        this.areaTeName = areaTeName;
    }

    public String getAreaTeTypeName() {
        return areaTeTypeName;
    }

    public void setAreaTeTypeName(String areaTeTypeName) {
        this.areaTeTypeName = areaTeTypeName;
    }

    public String getAreaTeTypeNameShort() {
        return areaTeTypeNameShort;
    }

    public void setAreaTeTypeNameShort(String areaTeTypeNameShort) {
        this.areaTeTypeNameShort = areaTeTypeNameShort;
    }

    public String getAreaCode() {
        return areaCode;
    }

    public void setAreaCode(String areaCode) {
        this.areaCode = areaCode;
    }

    public String getAreaBtiCode() {
        return areaBtiCode;
    }

    public void setAreaBtiCode(String areaBtiCode) {
        this.areaBtiCode = areaBtiCode;
    }

    public String getAreaName() {
        return areaName;
    }

    public void setAreaName(String areaName) {
        this.areaName = areaName;
    }

    public String getAreaTypeName() {
        return areaTypeName;
    }

    public void setAreaTypeName(String areaTypeName) {
        this.areaTypeName = areaTypeName;
    }

    public String getAreaTypeNameShort() {
        return areaTypeNameShort;
    }

    public void setAreaTypeNameShort(String areaTypeNameShort) {
        this.areaTypeNameShort = areaTypeNameShort;
    }

    public String getCityCode() {
        return cityCode;
    }

    public void setCityCode(String cityCode) {
        this.cityCode = cityCode;
    }

    public String getCityBtiCode() {
        return cityBtiCode;
    }

    public void setCityBtiCode(String cityBtiCode) {
        this.cityBtiCode = cityBtiCode;
    }

    public String getCityName() {
        return cityName;
    }

    public void setCityName(String cityName) {
        this.cityName = cityName;
    }

    public String getCityTypeName() {
        return cityTypeName;
    }

    public void setCityTypeName(String cityTypeName) {
        this.cityTypeName = cityTypeName;
    }

    public String getCityTypeNameShort() {
        return cityTypeNameShort;
    }

    public void setCityTypeNameShort(String cityTypeNameShort) {
        this.cityTypeNameShort = cityTypeNameShort;
    }

    public String getPlaceCode() {
        return placeCode;
    }

    public void setPlaceCode(String placeCode) {
        this.placeCode = placeCode;
    }

    public String getPlaceBtiCode() {
        return placeBtiCode;
    }

    public void setPlaceBtiCode(String placeBtiCode) {
        this.placeBtiCode = placeBtiCode;
    }

    public String getPlaceName() {
        return placeName;
    }

    public void setPlaceName(String placeName) {
        this.placeName = placeName;
    }

    public String getPlaceTypeName() {
        return placeTypeName;
    }

    public void setPlaceTypeName(String placeTypeName) {
        this.placeTypeName = placeTypeName;
    }

    public String getPlaceTypeNameShort() {
        return placeTypeNameShort;
    }

    public void setPlaceTypeNameShort(String placeTypeNameShort) {
        this.placeTypeNameShort = placeTypeNameShort;
    }

    public String getPlanCode() {
        return planCode;
    }

    public void setPlanCode(String planCode) {
        this.planCode = planCode;
    }

    public String getPlanBtiCode() {
        return planBtiCode;
    }

    public void setPlanBtiCode(String planBtiCode) {
        this.planBtiCode = planBtiCode;
    }

    public String getPlanName() {
        return planName;
    }

    public void setPlanName(String planName) {
        this.planName = planName;
    }

    public String getPlanTypeName() {
        return planTypeName;
    }

    public void setPlanTypeName(String planTypeName) {
        this.planTypeName = planTypeName;
    }

    public String getPlanTypeNameShort() {
        return planTypeNameShort;
    }

    public void setPlanTypeNameShort(String planTypeNameShort) {
        this.planTypeNameShort = planTypeNameShort;
    }

    public String getStreetCode() {
        return streetCode;
    }

    public void setStreetCode(String streetCode) {
        this.streetCode = streetCode;
    }

    public String getStreetName() {
        return streetName;
    }

    public void setStreetName(String streetName) {
        this.streetName = streetName;
    }

    public String getStreetOmkUm() {
        return streetOmkUm;
    }

    public void setStreetOmkUm(String streetOmkUm) {
        this.streetOmkUm = streetOmkUm;
    }

    public String getStreetTypeName() {
        return streetTypeName;
    }

    public void setStreetTypeName(String streetTypeName) {
        this.streetTypeName = streetTypeName;
    }

    public String getStreetTypeNameShort() {
        return streetTypeNameShort;
    }

    public void setStreetTypeNameShort(String streetTypeNameShort) {
        this.streetTypeNameShort = streetTypeNameShort;
    }

    public String getStreetBtiCode() {
        return streetBtiCode;
    }

    public void setStreetBtiCode(String streetBtiCode) {
        this.streetBtiCode = streetBtiCode;
    }

    public String getL1Type() {
        return l1Type;
    }

    public void setL1Type(String l1Type) {
        this.l1Type = l1Type;
    }

    public String getL1TypeShort() {
        return l1TypeShort;
    }

    public void setL1TypeShort(String l1TypeShort) {
        this.l1TypeShort = l1TypeShort;
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

    public String getL2TypeShort() {
        return l2TypeShort;
    }

    public void setL2TypeShort(String l2TypeShort) {
        this.l2TypeShort = l2TypeShort;
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

    public String getL3TypeShort() {
        return l3TypeShort;
    }

    public void setL3TypeShort(String l3TypeShort) {
        this.l3TypeShort = l3TypeShort;
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

    public LocalDateTime getCreateDate() {
        return createDate;
    }

    public void setCreateDate(LocalDateTime createDate) {
        this.createDate = createDate;
    }

    public LocalDateTime getUpdateDate() {
        return updateDate;
    }

    public void setUpdateDate(LocalDateTime updateDate) {
        this.updateDate = updateDate;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Addresses addresses = (Addresses) o;
        return globalId.equals(addresses.globalId);
    }

    @Override
    public int hashCode() {        
        return Objects.hashCode(this.id);
    }
}
