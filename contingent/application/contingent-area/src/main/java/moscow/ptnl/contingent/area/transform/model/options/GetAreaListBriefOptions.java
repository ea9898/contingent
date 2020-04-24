package moscow.ptnl.contingent.area.transform.model.options;

import java.util.EnumSet;

public enum GetAreaListBriefOptions implements OptionEnum {

    SHOW_ME("showME", EnumSet.allOf(ShowMeValues.class));

    private String keyName;

    private EnumSet<? extends OptionValuesEnum> possibleValues;

    GetAreaListBriefOptions(String keyName, EnumSet<? extends OptionValuesEnum> possibleValues) {
        this.keyName = keyName;
        this.possibleValues = possibleValues;
    }

    @Override
    public String getKeyName() {
        return keyName;
    }

    @Override
    public EnumSet<? extends OptionValuesEnum> getPossibleValues() {
        return possibleValues;
    }

    public enum ShowMeValues implements OptionValuesEnum {
        ALL, NONE;
    }
}
