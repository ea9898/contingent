package moscow.ptnl.contingent.nsi.domain.area;

import javax.persistence.Cacheable;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.validation.constraints.Size;
import java.io.Serializable;
import java.util.Objects;

@Entity
@Table(name = "NSI_ADDRESS_FORMING_ELEMENT")
@Cacheable
public class NsiAddressFormingElement implements Serializable {

    private static final long serialVersionUID = 5085904638016789913L;

    @Id
    @Column(name = "ID", unique = true, nullable = false)
    private Long id;

    @Column(name = "GLOBAL_ID")
    private Long globalId;

    @Size(max = 2)
    @Column(name = "AO_LEVEL")
    private String aoLevel;

    @Size(max = 11)
    @Column(name = "KOD_GIVZ")
    private String codeGivz;

    @Column(name = "REGION_ID")
    private Long regionId;

    @Size(max = 11)
    @Column(name = "REGION_CODE")
    private String regionCode;

    @Size(max = 4000)
    @Column(name = "REGION_NAME")
    private String regionName;

    @Column(name = "REGION_TE_ID")
    private Long regionTeId;

    @Size(max = 4)
    @Column(name = "REGION_TE_CODE")
    private String regionTeCode;

    @Size(max = 4000)
    @Column(name = "REGION_TE_NAME")
    private String regionTeName;

    @Size(max = 256)
    @Column(name = "REGION_TE_TYPENAME")
    private String regionTeTypeName;

    @Size(max = 50)
    @Column(name = "AREACODE_OMK_TE")
    private String areaCodeOmkTe;

    @Size(max = 4000)
    @Column(name = "AREA_TE_NAME")
    private String areaTeName;

    @Size(max = 128)
    @Column(name = "AREA_TE_TYPENAME")
    private String areaTeTypeName;

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

    @Size(max = 3)
    @Column(name = "PLANCODE")
    private String planCode;

    @Size(max = 11)
    @Column(name = "PLAN_BTI_CODE")
    private String planBtiCode;

    @Size(max = 4000)
    @Column(name = "PLAN_NAME")
    private String planName;

    @Size(max = 128)
    @Column(name = "PLAN_TYPENAME")
    private String planTypeName;

    @Size(max = 4)
    @Column(name = "STREETCODE")
    private String streetCode;

    @Size(max = 11)
    @Column(name = "STREET_BTI_CODE")
    private String streetBtiCode;

    @Size(max = 4000)
    @Column(name = "STREET_NAME")
    private String streetName;

    @Size(max = 10)
    @Column(name = "STREET_OMK_UM")
    private String streetOmkUm;

    @Size(max = 128)
    @Column(name = "STREET_TYPENAME")
    private String streetTypeName;

    @Size(max = 4000)
    @Column(name = "ADDRESS", nullable = false)
    private String address;

    public NsiAddressFormingElement() {
        super();
    }

    public NsiAddressFormingElement(Long id, Long globalId, String aoLevel, String codeGivz, String regionTeCode, String regionTeName,
                                    String regionTeTypeName, String areaCodeOmkTe, String areaTeName, String areaTeTypeName,
                                    String areaCode, String areaBtiCode, String areaName, String areaTypeName,
                                    String cityCode, String cityBtiCode, String cityName, String cityTypeName,
                                    String placeCode, String placeBtiCode, String placeName, String placeTypeName,
                                    String planCode, String planBtiCode, String planName, String planTypeName,
                                    String streetCode, String streetBtiCode, String streetName, String streetOmkUm,
                                    String streetTypeName, String address) {
        this.id = id;
        this.globalId = globalId;
        this.aoLevel = aoLevel;
        this.codeGivz = codeGivz;
        this.regionTeCode = regionTeCode;
        this.regionTeName = regionTeName;
        this.regionTeTypeName = regionTeTypeName;
        this.areaCodeOmkTe = areaCodeOmkTe;
        this.areaTeName = areaTeName;
        this.areaTeTypeName = areaTeTypeName;
        this.areaCode = areaCode;
        this.areaBtiCode = areaBtiCode;
        this.areaName = areaName;
        this.areaTypeName = areaTypeName;
        this.cityCode = cityCode;
        this.cityBtiCode = cityBtiCode;
        this.cityName = cityName;
        this.cityTypeName = cityTypeName;
        this.placeCode = placeCode;
        this.placeBtiCode = placeBtiCode;
        this.placeName = placeName;
        this.placeTypeName = placeTypeName;
        this.planCode = planCode;
        this.planBtiCode = planBtiCode;
        this.planName = planName;
        this.planTypeName = planTypeName;
        this.streetCode = streetCode;
        this.streetBtiCode = streetBtiCode;
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

    public String getCodeGivz() {
        return codeGivz;
    }

    public void setCodeGivz(String codeGivz) {
        this.codeGivz = codeGivz;
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

    public String getStreetCode() {
        return streetCode;
    }

    public void setStreetCode(String streetCode) {
        this.streetCode = streetCode;
    }

    public String getStreetBtiCode() {
        return streetBtiCode;
    }

    public void setStreetBtiCode(String streetBtiCode) {
        this.streetBtiCode = streetBtiCode;
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
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        NsiAddressFormingElement that = (NsiAddressFormingElement) o;
        return Objects.equals(id, that.id);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id);
    }
}
