package moscow.ptnl.contingent.domain.area.model.area;

public class AddressRegistry {

    private String addressString;

    private Region region;

    private RegionOMKTE regionOMKTE;

    private Area area;

    private AreaOMKTE areaOMKTE;

    private City city;

    private Place place;

    private Plan plan;

    private Street street;

    private Building building;

    private Long globalIdNsi;

    private String aoLevel;

    private String fiasGuid;

    private String codePostal;

    private String codeKLADR;

    private String nonActualAddress;

    public String getAddressString() {
        return addressString;
    }

    public void setAddressString(String addressString) {
        this.addressString = addressString;
    }

    public Region getRegion() {
        return region;
    }

    public void setRegion(Region region) {
        this.region = region;
    }

    public RegionOMKTE getRegionOMKTE() {
        return regionOMKTE;
    }

    public void setRegionOMKTE(RegionOMKTE regionOMKTE) {
        this.regionOMKTE = regionOMKTE;
    }

    public Area getArea() {
        return area;
    }

    public void setArea(Area area) {
        this.area = area;
    }

    public AreaOMKTE getAreaOMKTE() {
        return areaOMKTE;
    }

    public void setAreaOMKTE(AreaOMKTE areaOMKTE) {
        this.areaOMKTE = areaOMKTE;
    }

    public City getCity() {
        return city;
    }

    public void setCity(City city) {
        this.city = city;
    }

    public Place getPlace() {
        return place;
    }

    public void setPlace(Place place) {
        this.place = place;
    }

    public Plan getPlan() {
        return plan;
    }

    public void setPlan(Plan plan) {
        this.plan = plan;
    }

    public Street getStreet() {
        return street;
    }

    public void setStreet(Street street) {
        this.street = street;
    }

    public Building getBuilding() {
        return building;
    }

    public void setBuilding(Building building) {
        this.building = building;
    }

    public Long getGlobalIdNsi() {
        return globalIdNsi;
    }

    public void setGlobalIdNsi(Long globalIdNsi) {
        this.globalIdNsi = globalIdNsi;
    }

    public String getAoLevel() {
        return aoLevel;
    }

    public void setAoLevel(String aoLevel) {
        this.aoLevel = aoLevel;
    }

    public String getFiasGuid() {
        return fiasGuid;
    }

    public void setFiasGuid(String fiasGuid) {
        this.fiasGuid = fiasGuid;
    }

    public String getCodePostal() {
        return codePostal;
    }

    public void setCodePostal(String codePostal) {
        this.codePostal = codePostal;
    }

    public String getCodeKLADR() {
        return codeKLADR;
    }

    public void setCodeKLADR(String codeKLADR) {
        this.codeKLADR = codeKLADR;
    }

    public String getNonActualAddress() {
        return nonActualAddress;
    }

    public void setNonActualAddress(String nonActualAddress) {
        this.nonActualAddress = nonActualAddress;
    }
}
