package moscow.ptnl.contingent.area.model.area;

import java.util.Objects;

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

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        NotNsiAddress that = (NotNsiAddress) o;
        return Objects.equals(levelParentId, that.levelParentId) &&
                Objects.equals(parentId, that.parentId) &&
                Objects.equals(houseType, that.houseType) &&
                Objects.equals(house, that.house) &&
                Objects.equals(buildingType, that.buildingType) &&
                Objects.equals(building, that.building) &&
                Objects.equals(constructionType, that.constructionType) &&
                Objects.equals(construction, that.construction);
    }

    @Override
    public int hashCode() {
        return Objects.hash(levelParentId, parentId, houseType, house, buildingType, building, constructionType, construction);
    }
}
