package moscow.ptnl.contingent.nsi.pushaccepter.xmlparsing;

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlType;

@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "", propOrder = {
        "value",
        "groupvalue"
})
public class Values {
    public Value value;
    public Groupvalue groupvalue;
}
