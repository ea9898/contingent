package moscow.ptnl.contingent.domain.area.model.area;

public class Building {

    protected Building.House house;

    protected Building.Build build;

    protected Building.Construction construction;

    protected String clazz;

    protected String geoData;

    protected Long numberOfStoreys;

    protected Long yearOfConstruction;

    protected Boolean emergency;

    protected String cadastralNumber;

    protected String fiasGuid;

    protected Long unom;

    public static class House {

        protected String name;

        protected Names type;

    }

    public static class Build {

        protected String name;

        protected Names type;

    }

    public static class Construction {

        protected String name;

        protected Names type;

    }



}
