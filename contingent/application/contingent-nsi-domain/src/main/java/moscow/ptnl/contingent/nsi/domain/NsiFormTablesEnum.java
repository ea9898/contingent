package moscow.ptnl.contingent.nsi.domain;

public enum NsiFormTablesEnum {

    ADDRESSES, NSI_ADDRESS_FORMING_ELEMENT;

    public static NsiFormTablesEnum findByName(String name) {
        for (NsiFormTablesEnum suit : NsiFormTablesEnum.values()) {
            if (name.equals(suit.name())) {
                return suit;
            }
        }
        return null;
    }
}
