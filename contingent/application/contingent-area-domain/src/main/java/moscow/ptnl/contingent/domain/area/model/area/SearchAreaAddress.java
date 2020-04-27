package moscow.ptnl.contingent.domain.area.model.area;

import moscow.ptnl.contingent.domain.area.entity.Addresses;

/*
Выходной объекта метода searchArea из слоя Service
 */
public class SearchAreaAddress {

    private String aoLevel;
    private long globalIdNsi;
    private String regionOMKTEcode;
    private String areaOMKTEcode;
    private String areaCode;
    private String cityCode;
    private String placeCode;
    private String planCode;
    private String streetCode;
    private String house;
    private String build;
    private String construction;

    public SearchAreaAddress() {
    }

    public SearchAreaAddress(Addresses address) {
        this.aoLevel = address.getAoLevel();
        this.globalIdNsi = address.getGlobalId();
        this.regionOMKTEcode = address.getRegionTeCode();
        this.areaOMKTEcode = address.getAreaCodeOmkTe();
        this.areaCode = address.getAreaCode();
        this.cityCode = address.getCityCode();
        this.placeCode = address.getPlaceCode();
        this.planCode = address.getPlanCode();
        this.streetCode = address.getStreetCode();
    }

    public SearchAreaAddress(SearchAreaAddress address) {
        this.aoLevel = address.aoLevel;
        this.globalIdNsi = address.globalIdNsi;
        this.regionOMKTEcode = address.regionOMKTEcode;
        this.areaOMKTEcode = address.areaOMKTEcode;
        this.areaCode = address.areaCode;
        this.cityCode = address.cityCode;
        this.placeCode = address.placeCode;
        this.planCode = address.planCode;
        this.streetCode = address.streetCode;
        this.house = address.house;
        this.build = address.build;
        this.construction = address.construction;
    }

    public void setRegionOMKTEcode(String regionOMKTEcode) {
        this.regionOMKTEcode = regionOMKTEcode;
    }

    public void setAreaOMKTEcode(String areaOMKTEcode) {
        this.areaOMKTEcode = areaOMKTEcode;
    }

    public String getAoLevel() {
        return aoLevel;
    }

    public long getGlobalIdNsi() {
        return globalIdNsi;
    }

    public void setGlobalIdNsi(long globalIdNsi) {
        this.globalIdNsi = globalIdNsi;
    }

    public String getRegionOMKTEcode() {
        return regionOMKTEcode;
    }

    public String getAreaOMKTEcode() {
        return areaOMKTEcode;
    }

    public String getAreaCode() {
        return areaCode;
    }

    public String getCityCode() {
        return cityCode;
    }

    public String getPlaceCode() {
        return placeCode;
    }

    public String getPlanCode() {
        return planCode;
    }

    public String getStreetCode() {
        return streetCode;
    }

    public String getHouse() {
        return house;
    }

    public String getBuild() {
        return build;
    }

    public String getConstruction() {
        return construction;
    }
}
