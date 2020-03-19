package moscow.ptnl.contingent.domain.area.model.area;

import java.util.Arrays;

public enum AddressDetailsType {

    //Тип номера дома, владения
    HOUSE(AddressDetailsElementType.HOUSE, "дом"),
    OWNERSHIP(AddressDetailsElementType.HOUSE, "владение"),
    HOUSEHOLD(AddressDetailsElementType.HOUSE, "домовладение"),
    //Тип номера корпуса
    BUILDING(AddressDetailsElementType.BUILDING, "корпус"),
    //Тип номера строения, сооружения
    CONSTRUCTION(AddressDetailsElementType.CONSTRUCTION, "строение"),
    STRUCTURE(AddressDetailsElementType.CONSTRUCTION, "сооружение");

    private AddressDetailsElementType elementType;
    private String value;

    AddressDetailsType(AddressDetailsElementType elementType, String value) {
        this.elementType = elementType;
        this.value = value;
    }

    public static AddressDetailsType getAddressDetailsType(String value, AddressDetailsElementType elementType) {
        return Arrays.stream(AddressDetailsType.values())
                .filter(t -> t.value.equals(value) && t.elementType.equals(elementType))
                .findFirst().orElse(null);
    }
}
