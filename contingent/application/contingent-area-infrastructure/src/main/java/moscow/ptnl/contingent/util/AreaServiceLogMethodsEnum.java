package moscow.ptnl.contingent.util;

import java.util.Arrays;

public enum AreaServiceLogMethodsEnum {
    CREATE_PRIMARY_AREA("createPrimaryArea"),
    CREATE_DEPENDENT_AREA("createDependentArea"),
    UPDATE_PRIMARY_AREA("updatePrimaryArea"),
    UPDATE_DEPENDENT_AREA("updateDependentArea"),
    SET_MEDICAL_EMPLOYEE_ON_AREA("setMedicalEmployeeOnArea"),
    ADD_AREA_ADDRESS("addAreaAddress"),
    ADD_MO_ADDRESS("addMoAddress"),
    DEL_AREA_ADDRESS("delAreaAddress"),
    DEL_MO_ADDRESS("delMoAddress"),
    INITIATE_CREATE_PRIMARY_AREA("initiateCreatePrimaryArea"),
    INITIATE_ADD_AREA_ADDRESS("initiateAddAreaAddress"),
    INITIATE_ADD_MO_ADDRESS("initiateAddMoAddress"),
    ARCHIVE_AREA("archiveArea"),
    RESTORE_AREA("restoreArea");

    private final String name;

    AreaServiceLogMethodsEnum(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }

    public static boolean contains(String name) {
        return Arrays.stream(AreaServiceLogMethodsEnum.values()).anyMatch(method -> method.getName().equals(name));
    }
}
