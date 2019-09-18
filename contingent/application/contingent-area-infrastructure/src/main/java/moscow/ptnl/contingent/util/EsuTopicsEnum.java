package moscow.ptnl.contingent.util;

/**
 * Топики Есу
 */

public enum  EsuTopicsEnum {

    ATTACHMENT_PRIMARY("AttachmentPrimary"),
    JOB_EXECUTION_INFO_MSG("JEMsg"),
    ATTACH_TO_DEPENDENT_AREA("AttachToDependentArea");

    private final String name;

    EsuTopicsEnum(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }
}