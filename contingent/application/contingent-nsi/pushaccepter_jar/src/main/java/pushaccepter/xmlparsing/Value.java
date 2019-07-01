package pushaccepter.xmlparsing;

import javax.xml.bind.annotation.*;

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
