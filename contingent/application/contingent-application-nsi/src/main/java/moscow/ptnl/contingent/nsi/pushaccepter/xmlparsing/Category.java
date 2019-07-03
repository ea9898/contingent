package moscow.ptnl.contingent.nsi.pushaccepter.xmlparsing;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlValue;

@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "", propOrder = {
        "value"
})
public class Category {

    @XmlValue
    public short value;
    @XmlAttribute(name = "idHier")
    public String idHier;
    @XmlAttribute(name = "nameHier")
    public String nameHier;
}