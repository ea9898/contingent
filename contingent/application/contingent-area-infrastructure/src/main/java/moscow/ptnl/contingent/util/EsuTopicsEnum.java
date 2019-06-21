package moscow.ptnl.contingent.util;

/**
 * Топики Есу
 */

public enum  EsuTopicsEnum {

    ATTACHMENT_PRIMARY("AttachmentPrimary");

    private final String name;

    EsuTopicsEnum(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }
}