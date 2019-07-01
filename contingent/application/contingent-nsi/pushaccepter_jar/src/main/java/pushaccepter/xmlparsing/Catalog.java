package pushaccepter.xmlparsing;

import javax.xml.bind.annotation.*;

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
