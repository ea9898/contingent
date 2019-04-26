package moscow.ptnl.contingent.area.entity.nsi;

import javax.persistence.Cacheable;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.validation.constraints.Size;
import java.io.Serializable;

@Entity
@Table(name = "ADDRESS_FORMING_ELEMENT")
@Cacheable
public class AddressFormingElement implements Serializable {

    private static final long serialVersionUID = 5085904638016789913L;

    @Id
	@Column(name = "GLOBAL_ID", unique = true, nullable = false)
	private Long globalId;

    @Size(max = 4)
	@Column(name = "REGION_TE_CODE")
	private String regionTeCode;

    @Size(max = 4000)
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

    @Size(max = 2)
	@Column(name = "AOLEVEL")
	private String aoLevel;

    @Size(max = 4000)
	@Column(name = "ADDRESS", nullable = false)
	private String address;

    @Size(max = 4000)
	@Column(name = "REGION_TE_NAME")
	private String regionTeName;

    @Size(max = 4000)
	@Column(name = "AREA_TE_NAME")
	private String areaTeName;

    @Size(max = 4000)
	@Column(name = "AREA_NAME")
	private String areaName;

    @Size(max = 4000)
	@Column(name = "CITY_NAME")
	private String cityName;

    @Size(max = 4000)
	@Column(name = "PLACE_NAME")
	private String placeName;

    @Size(max = 4000)
	@Column(name = "PLAN_NAME")
	private String planName;

    @Size(max = 4000)
	@Column(name = "STREET_NAME")
	private String streetName;

    @Size(max = 10)
	@Column(name = "STREET_OMK_UM")
	private String streetOmkUm;

    @Column(name = "STREET_ID")
    private Long streetId;

    public Long getGlobalId() {
        return globalId;
    }

    public void setGlobalId(Long globalId) {
        this.globalId = globalId;
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

    public String getAoLevel() {
        return aoLevel;
    }

    public void setAoLevel(String aoLevel) {
        this.aoLevel = aoLevel;
    }

    public String getAddress() {
        return address;
    }

    public void setAddress(String address) {
        this.address = address;
    }

    public String getRegionTeName() {
        return regionTeName;
    }

    public void setRegionTeName(String regionTeName) {
        this.regionTeName = regionTeName;
    }

    public String getAreaTeName() {
        return areaTeName;
    }

    public void setAreaTeName(String areaTeName) {
        this.areaTeName = areaTeName;
    }

    public String getAreaName() {
        return areaName;
    }

    public void setAreaName(String areaName) {
        this.areaName = areaName;
    }

    public String getCityName() {
        return cityName;
    }

    public void setCityName(String cityName) {
        this.cityName = cityName;
    }

    public String getPlaceName() {
        return placeName;
    }

    public void setPlaceName(String placeName) {
        this.placeName = placeName;
    }

    public String getPlanName() {
        return planName;
    }

    public void setPlanName(String planName) {
        this.planName = planName;
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

    public Long getStreetId() {
        return streetId;
    }

    public void setStreetId(Long streetId) {
        this.streetId = streetId;
    }
}
