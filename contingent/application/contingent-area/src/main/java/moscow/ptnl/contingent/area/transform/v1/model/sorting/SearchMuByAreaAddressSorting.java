package moscow.ptnl.contingent.area.transform.v1.model.sorting;

import moscow.ptnl.contingent.area.transform.SortingFieldEnum;
import moscow.ptnl.util.Strings;

public enum SearchMuByAreaAddressSorting implements SortingFieldEnum {

    MO_ID("ar.moId"),
    MU_ID("ar.muId");

    private String fieldName;

    SearchMuByAreaAddressSorting(String fieldName) {
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
