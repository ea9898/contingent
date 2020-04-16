package moscow.ptnl.contingent.domain.area.model.area;

public class Building {

    private Building.House house;

    private Building.Build build;

    private Building.Construction construction;

    private String clazz;

    private String geoData;

    private Long numberOfStoreys;

    private Long yearOfConstruction;

    private Boolean emergency;

    private String cadastralNumber;

    private String fiasGuid;

    private Long unom;

    public House getHouse() {
        return house;
    }

    public void setHouse(House house) {
        this.house = house;
    }

    public Build getBuild() {
        return build;
    }

    public void setBuild(Build build) {
        this.build = build;
    }

    public Construction getConstruction() {
        return construction;
    }

    public void setConstruction(Construction construction) {
        this.construction = construction;
    }

    public String getClazz() {
        return clazz;
    }

    public void setClazz(String clazz) {
        this.clazz = clazz;
    }

    public String getGeoData() {
        return geoData;
    }

    public void setGeoData(String geoData) {
        this.geoData = geoData;
    }

    public Long getNumberOfStoreys() {
        return numberOfStoreys;
    }

    public void setNumberOfStoreys(Long numberOfStoreys) {
        this.numberOfStoreys = numberOfStoreys;
    }

    public Long getYearOfConstruction() {
        return yearOfConstruction;
    }

    public void setYearOfConstruction(Long yearOfConstruction) {
        this.yearOfConstruction = yearOfConstruction;
    }

    public Boolean getEmergency() {
        return emergency;
    }

    public void setEmergency(Boolean emergency) {
        this.emergency = emergency;
    }

    public String getCadastralNumber() {
        return cadastralNumber;
    }

    public void setCadastralNumber(String cadastralNumber) {
        this.cadastralNumber = cadastralNumber;
    }

    public String getFiasGuid() {
        return fiasGuid;
    }

    public void setFiasGuid(String fiasGuid) {
        this.fiasGuid = fiasGuid;
    }

    public Long getUnom() {
        return unom;
    }

    public void setUnom(Long unom) {
        this.unom = unom;
    }

    public static class House {

        private String name;

        private Names type;

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
    }

    public static class Build {

        private String name;

        private Names type;

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
    }

    public static class Construction {

        private String name;

        private Names type;

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
    }



}
