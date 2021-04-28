package moscow.ptnl.contingent.area.transform;

import java.util.EnumSet;

public interface OptionEnum {

    String getKeyName();

    EnumSet<? extends OptionValuesEnum> getPossibleValues();

    interface OptionValuesEnum {
    }
}
