package pushaccepter.xmlparsing;

import javax.xml.bind.annotation.*;

/**
 * Created by rudenko_ae on 26.04.2017.
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "",propOrder = {
        "catalog"
})
@XmlRootElement(name = "package")
public class Package {
    @XmlElement(required = true)
    public Catalog catalog;
    @XmlAttribute(name = "count")
    public Byte count;
    @XmlAttribute(name = "id")
    public String id;
}
