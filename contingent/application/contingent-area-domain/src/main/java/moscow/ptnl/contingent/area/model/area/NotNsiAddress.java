package moscow.ptnl.contingent.area.model.area;

public class NotNsiAddress {

    private Integer levelParentId;

    private Long parentId;

    private String houseType;

    private String house;

    private String buildingType;

    private String building;

    private String constructionType;

    private String construction;

    public Integer getLevelParentId() {
        return levelParentId;
    }

    public void setLevelParentId(Integer levelParentId) {
        this.levelParentId = levelParentId;
    }

    public Long getParentId() {
        return parentId;
    }

    public void setParentId(Long parentId) {
        this.parentId = parentId;
    }

    public String getHouseType() {
        return houseType;
    }

    public void setHouseType(String houseType) {
        this.houseType = houseType;
    }

    public String getHouse() {
        return house;
    }

    public void setHouse(String house) {
        this.house = house;
    }

    public String getBuildingType() {
        return buildingType;
    }

    public void setBuildingType(String buildingType) {
        this.buildingType = buildingType;
    }

    public String getBuilding() {
        return building;
    }

    public void setBuilding(String building) {
        this.building = building;
    }

    public String getConstructionType() {
        return constructionType;
    }

    public void setConstructionType(String constructionType) {
        this.constructionType = constructionType;
    }

    public String getConstruction() {
        return construction;
    }

    public void setConstruction(String construction) {
        this.construction = construction;
    }
}
