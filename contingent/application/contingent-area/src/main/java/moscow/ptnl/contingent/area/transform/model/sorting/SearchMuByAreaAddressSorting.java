package moscow.ptnl.contingent.area.transform.model.sorting;

import moscow.ptnl.util.Strings;

public enum SearchMuByAreaAddressSorting implements SortingFieldEnum {

    MO_ID("moId"),
    MU_ID("muId");

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
