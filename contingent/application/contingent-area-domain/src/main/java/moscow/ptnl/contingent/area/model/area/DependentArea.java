package moscow.ptnl.contingent.area.model.area;

public class DependentArea {

    private Long id;

    private Long moId;

    private Long muId;

    private Long gender;

    private Integer ageMin;

    private Integer ageMax;

    private Integer ageMMin;

    private Integer ageMMax;

    private Integer ageWMin;

    private Integer ageWMax;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getMoId() {
        return moId;
    }

    public void setMoId(Long moId) {
        this.moId = moId;
    }

    public Long getMuId() {
        return muId;
    }

    public void setMuId(Long muId) {
        this.muId = muId;
    }

    public Long getGender() {
        return gender;
    }

    public void setGender(Long gender) {
        this.gender = gender;
    }

    public Integer getAgeMin() {
        return ageMin;
    }

    public void setAgeMin(Integer ageMin) {
        this.ageMin = ageMin;
    }

    public Integer getAgeMax() {
        return ageMax;
    }

    public void setAgeMax(Integer ageMax) {
        this.ageMax = ageMax;
    }

    public Integer getAgeMMin() {
        return ageMMin;
    }

    public void setAgeMMin(Integer ageMMin) {
        this.ageMMin = ageMMin;
    }

    public Integer getAgeMMax() {
        return ageMMax;
    }

    public void setAgeMMax(Integer ageMMax) {
        this.ageMMax = ageMMax;
    }

    public Integer getAgeWMin() {
        return ageWMin;
    }

    public void setAgeWMin(Integer ageWMin) {
        this.ageWMin = ageWMin;
    }

    public Integer getAgeWMax() {
        return ageWMax;
    }

    public void setAgeWMax(Integer ageWMax) {
        this.ageWMax = ageWMax;
    }
}
