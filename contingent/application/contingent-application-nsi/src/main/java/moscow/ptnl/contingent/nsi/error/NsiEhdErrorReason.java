package moscow.ptnl.contingent.nsi.error;

import moscow.ptnl.contingent.area.error.ErrorReason;

public enum NsiEhdErrorReason implements ErrorReason {

    UNEXPECTED_ERROR("E000", "Непредвиденная ошибка. %s"),
    NSI_EHD_ERROR("E001", "Ошибка получения данных из НСИ. %s"),
    CATALOG_ID_NOT_FOUND("E002", "Неверное имя справочника. %s"),
    ;

    private final String description;
    private final String code;

    NsiEhdErrorReason(String code, String description) {
        this.code = code;
        this.description = description;
    }

    public String getDescription() {
        return description;
    }
    public String getCode() {
        return code;
    }

}
