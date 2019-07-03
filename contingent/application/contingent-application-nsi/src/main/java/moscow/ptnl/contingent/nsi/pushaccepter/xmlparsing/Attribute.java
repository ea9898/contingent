package moscow.ptnl.contingent.nsi.pushaccepter.xmlparsing;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

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
