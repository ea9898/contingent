package moscow.ptnl.contingent.nsi.pushaccepter.xmlparsing;

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.XmlValue;

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