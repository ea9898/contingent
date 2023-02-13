package moscow.ptnl.contingent.nsi.pushaccepter.xmlparsing;

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlType;

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
