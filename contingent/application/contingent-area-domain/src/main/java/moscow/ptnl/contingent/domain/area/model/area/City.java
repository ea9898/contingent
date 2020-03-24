package moscow.ptnl.contingent.domain.area.model.area;

public class City {

    private String codeOMKTM;

    private String codeBTI;

    private String fiasGuid;

    private String id;

    private String code;

    private String name;

    private Names type;

    public String getCodeOMKTM() { return codeOMKTM; }

    public void setCodeOMKTM(String codeOMKTM) { this.codeOMKTM = codeOMKTM; }

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
