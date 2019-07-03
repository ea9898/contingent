package moscow.ptnl.contingent.nsi.pushaccepter.xmlparsing;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;
import java.util.List;

@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "", propOrder = {
        "item"
})
public class Groupvalue {
    public List<Item> item;
}