package moscow.ptnl.contingent.nsi.pushaccepter.xmlparsing;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

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
