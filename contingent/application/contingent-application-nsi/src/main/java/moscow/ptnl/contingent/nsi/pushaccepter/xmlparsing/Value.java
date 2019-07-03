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
public class Value {

    @XmlValue
    public String value;
    @XmlAttribute(name = "id")
    public String id;
}
