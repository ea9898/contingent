package moscow.ptnl.contingent.domain.area.model.sysop;

public enum SysopMethodType {

    INITIATE_ADD_AREA_ADDRESS("initiateAddAreaAddress"),
    INITIATE_ADD_MO_ADDRESS("initiateAddMoAddress"),
    INITIATE_CREATE_PRIMARY_AREA("initiateCreatePrimaryArea");

    private String value;

    SysopMethodType(String value) {
        //На всякий случай, чтобы не превысить длину поля в БД
        this.value = value.substring(0, Math.min(value.length(), 255));
    }

    public String getValue() {
        return value;
    }
}
