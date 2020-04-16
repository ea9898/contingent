package moscow.ptnl.contingent.domain.area.model.area;

public class Street {

    private String codeOMKUM;

    private String codeBTI;

    private String fiasGuid;

    private String id;

    private String code;

    private String name;

    private Names type;

    public String getCodeOMKUM() { return codeOMKUM; }

    public void setCodeOMKUM(String codeOMKUM) { this.codeOMKUM = codeOMKUM; }

    public String getCodeBTI() {
        return codeBTI;
    }

    public void setCodeBTI(String codeBTI) {
        this.codeBTI = codeBTI;
    }

    public String getFiasGuid() {
        return fiasGuid;
    }

    public void setFiasGuid(String fiasGuid) {
        this.fiasGuid = fiasGuid;
    }

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getCode() {
        return code;
    }

    public void setCode(String code) {
        this.code = code;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Names getType() {
        return type;
    }

    public void setType(Names type) {
        this.type = type;
    }
}
