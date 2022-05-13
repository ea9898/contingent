package moscow.ptnl.contingent.area.transform.v3.model.sorting;

import moscow.ptnl.contingent.area.transform.SortingFieldEnum;
import moscow.ptnl.util.Strings;

public enum GetAreaHistorySorting implements SortingFieldEnum {

    UPDATE_DATE("change_date");

    private final String fieldName;

    GetAreaHistorySorting(String fieldName) {
        this.fieldName = fieldName;
    }

    public String getFieldName() {
        return fieldName;
    }

    @Override
    public String toString() {
        return Strings.toCamelCase(this.name());
    }
}
