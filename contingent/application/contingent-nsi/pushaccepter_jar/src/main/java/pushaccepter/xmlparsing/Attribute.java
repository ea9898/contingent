package pushaccepter.xmlparsing;

import javax.xml.bind.annotation.*;

@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "", propOrder = {
        "values"
})
public class Attribute {
    @XmlElement(required = true)
    public Values values;
    @XmlAttribute(name = "name")
    public String name;
    @XmlAttribute(name = "field_id")
    public Integer fieldId;
    @XmlAttribute(name = "type")
    public String type;
}
