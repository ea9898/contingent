package moscow.ptnl.contingent.nsi.pushaccepter.xmlparsing;

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlType;

@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "", propOrder = {
        "categories",
        "data"
})
public class Catalog {
    @XmlAttribute(name = "name")
    public String name;
    @XmlElement(required = true)
    public Data data;
    @XmlElement(required = true)
    public Categories categories;
}
