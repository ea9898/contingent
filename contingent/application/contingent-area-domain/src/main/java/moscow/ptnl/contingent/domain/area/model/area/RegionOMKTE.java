package moscow.ptnl.contingent.domain.area.model.area;

public class RegionOMKTE {

    private String id;

    private String code;

    private String name;

    private String shortName;

    private Names type;

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getCode() {
        return code;
    }

    public void setCode(String code) {
        this.code = code;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Names getType() {
        return type;
    }

    public void setType(Names type) {
        this.type = type;
    }

    public String getShortName() { return shortName; }

    public void setShortName(String shortName) { this.shortName = shortName; }
}
