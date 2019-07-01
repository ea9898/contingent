package pushaccepter.xmlparsingS;

import javax.xml.bind.annotation.*;

@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "")
public class Attribute {
    public Integer id;
    public Integer typeId;
    public String name;
    public String type;
    public String isPrimaryKey;
    public String isEdit;
    public String isReq;
    public String fieldMask;
    public String tehName;
    public Integer maxLength;
    public String maxLengthDecimal;
    public String dictId;
    public String refCatalog;
    public String isDeleted;
    public String isDeletedTmp;
    public String isMulti;
    public Table table;
}
