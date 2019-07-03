package moscow.ptnl.contingent.nsi.pushaccepter.xmlparsing;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

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
