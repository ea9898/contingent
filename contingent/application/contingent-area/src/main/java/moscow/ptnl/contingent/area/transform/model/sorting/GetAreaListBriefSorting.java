package moscow.ptnl.contingent.area.transform.model.sorting;

import moscow.ptnl.util.Strings;

public enum GetAreaListBriefSorting implements SortingFieldEnum {

    ID("id"),
    MO_ID("moId"),
    MU_ID("muId"),
    NUMBER("number"),
    AREA_TYPE_CODE("areaType.code"),
    ARCHIVE("archived");

    private String fieldName;

    GetAreaListBriefSorting(String fieldName) {
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
