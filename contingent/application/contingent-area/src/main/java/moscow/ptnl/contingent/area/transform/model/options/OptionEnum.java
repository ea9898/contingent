package moscow.ptnl.contingent.area.transform.model.options;

import java.util.EnumSet;

public interface OptionEnum {

    String getKeyName();

    EnumSet<? extends OptionValuesEnum> getPossibleValues();

    interface OptionValuesEnum {
    }
}
