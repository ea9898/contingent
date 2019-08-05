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
@Table(name = "ADDRESS_FORMING_ELEMENT")
@Cacheable
public class AddressFormingElement implements Serializable {

    private static final long serialVersionUID = 5085904638016789913L;

    @Id
    @Column(name = "ID", unique = true, nullable = false)
    private Long id;

    @Column(name = "GLOBAL_ID")
    private Long globalId;

    @Size(max = 2)
    @Column(name = "AO_LEVEL")
    private String aoLevel;

    @Column(name = "REGION_TE_ID")
    private Long regionTeId;

    @Size(max = 4)
    @Column(name = "REGION_TE_CODE")
    private String regionTeCode;

    @Size(max = 4000)
    @Column(name = "REGION_TE_NAME")
    private String regionTeName;

    @Size(max = 256)
    @Column(name = "REGION_TE_TYPE_NAME")
    private String regionTeTypeName;

    @Column(name = "AREA_TE_ID")
    private Long areaTeId;

    @Size(max = 50)
    @Column(name = "AREA_CODE_OMK_TE")
    private String areaCodeOmkTe;

    @Size(max = 4000)
    @Column(name = "AREA_TE_NAME")
    private String areaTeName;

    @Size(max = 128)
    @Column(name = "AREA_TE_TYPE_NAME")
    private String areaTeTypeName;

    @Column(name = "AREA_ID")
    private Long areaId;

    @Size(max = 3)
    @Column(name = "AREA_CODE")
    private String areaCode;

    @Size(max = 4000)
    @Column(name = "AREA_NAME")
    private String areaName;

    @Size(max = 128)
    @Column(name = "AREA_TYPE_NAME")
    private String areaTypeName;

    @Column(name = "CITY_ID")
    private Long cityId;

    @Size(max = 3)
    @Column(name = "CITY_CODE")
    private String cityCode;

    @Size(max = 4000)
    @Column(name = "CITY_NAME")
    private String cityName;

    @Size(max = 128)
    @Column(name = "CITY_TYPE_NAME")
    private String cityTypeName;

    @Column(name = "PLACE_ID")
    private Long placeId;

    @Size(max = 3)
    @Column(name = "PLACE_CODE")
    private String placeCode;

    @Size(max = 4000)
    @Column(name = "PLACE_NAME")
    private String placeName;

    @Size(max = 128)
    @Column(name = "PLACE_TYPE_NAME")
    private String placeTypeName;

    @Column(name = "PLAN_ID")
    private Long planId;

    @Size(max = 3)
    @Column(name = "PLAN_CODE")
    private String planCode;

    @Size(max = 4000)
    @Column(name = "PLAN_NAME")
    private String planName;

    @Size(max = 128)
    @Column(name = "PLAN_TYPE_NAME")
    private String planTypeName;

    @Column(name = "STREET_ID")
    private Long streetId;

    @Size(max = 4)
    @Column(name = "STREET_CODE")
    private String streetCode;

    @Size(max = 4000)
    @Column(name = "STREET_NAME")
    private String streetName;

    @Size(max = 10)
    @Column(name = "STREET_OMK_UM")
    private String streetOmkUm;

    @Size(max = 128)
    @Column(name = "STREET_TYPE_NAME")
    private String streetTypeName;

    @Size(max = 4000)
    @Column(name = "ADDRESS")
    private String address;

    public AddressFormingElement() {
        super();
    }

    public AddressFormingElement(Long id, Long globalId, String aoLevel, Long regionTeId, String regionTeCode, String regionTeName, String regionTeTypeName, Long areaTeId, String areaCodeOmkTe, String areaTeName, String areaTeTypeName, Long areaId, String areaCode, String areaName, String areaTypeName, Long cityId, String cityCode, String cityName, String cityTypeName, Long placeId, String placeCode, String placeName, String placeTypeName, Long planId, String planCode, String planName, String planTypeName, Long streetId, String streetCode, String streetName, String streetOmkUm, String streetTypeName, String address) {
        this.id = id;
        this.globalId = globalId;
        this.aoLevel = aoLevel;
        this.regionTeId = regionTeId;
        this.regionTeCode = regionTeCode;
        this.regionTeName = regionTeName;
        this.regionTeTypeName = regionTeTypeName;
        this.areaTeId = areaTeId;
        this.areaCodeOmkTe = areaCodeOmkTe;
        this.areaTeName = areaTeName;
        this.areaTeTypeName = areaTeTypeName;
        this.areaId = areaId;
        this.areaCode = areaCode;
        this.areaName = areaName;
        this.areaTypeName = areaTypeName;
        this.cityId = cityId;
        this.cityCode = cityCode;
        this.cityName = cityName;
        this.cityTypeName = cityTypeName;
        this.placeId = placeId;
        this.placeCode = placeCode;
        this.placeName = placeName;
        this.placeTypeName = placeTypeName;
        this.planId = planId;
        this.planCode = planCode;
        this.planName = planName;
        this.planTypeName = planTypeName;
        this.streetId = streetId;
        this.streetCode = streetCode;
        this.streetName = streetName;
        this.streetOmkUm = streetOmkUm;
        this.streetTypeName = streetTypeName;
        this.address = address;
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

    public String getAoLevel() {
        return aoLevel;
    }

    public void setAoLevel(String aoLevel) {
        this.aoLevel = aoLevel;
    }

    public Long getRegionTeId() {
        return regionTeId;
    }

    public void setRegionTeId(Long regionTeId) {
        this.regionTeId = regionTeId;
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

    public Long getAreaTeId() {
        return areaTeId;
    }

    public void setAreaTeId(Long areaTeId) {
        this.areaTeId = areaTeId;
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

    public Long getAreaId() {
        return areaId;
    }

    public void setAreaId(Long areaId) {
        this.areaId = areaId;
    }

    public String getAreaCode() {
        return areaCode;
    }

    public void setAreaCode(String areaCode) {
        this.areaCode = areaCode;
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

    public Long getCityId() {
        return cityId;
    }

    public void setCityId(Long cityId) {
        this.cityId = cityId;
    }

    public String getCityCode() {
        return cityCode;
    }

    public void setCityCode(String cityCode) {
        this.cityCode = cityCode;
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

    public Long getPlaceId() {
        return placeId;
    }

    public void setPlaceId(Long placeId) {
        this.placeId = placeId;
    }

    public String getPlaceCode() {
        return placeCode;
    }

    public void setPlaceCode(String placeCode) {
        this.placeCode = placeCode;
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

    public Long getPlanId() {
        return planId;
    }

    public void setPlanId(Long planId) {
        this.planId = planId;
    }

    public String getPlanCode() {
        return planCode;
    }

    public void setPlanCode(String planCode) {
        this.planCode = planCode;
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

    public Long getStreetId() {
        return streetId;
    }

    public void setStreetId(Long streetId) {
        this.streetId = streetId;
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
        if (obj != null && obj instanceof AddressFormingElement) {
            return ((AddressFormingElement) obj).getId().equals(this.id);
        }
        return false;
    }

    @Override
    public int hashCode() {        
        return Objects.hashCode(this.id);
    }
}
