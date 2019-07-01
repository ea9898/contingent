package pushaccepter.xmlparsing;

import javax.xml.bind.annotation.*;

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