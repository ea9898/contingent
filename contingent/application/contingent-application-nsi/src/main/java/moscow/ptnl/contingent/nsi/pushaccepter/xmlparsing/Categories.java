package moscow.ptnl.contingent.nsi.pushaccepter.xmlparsing;

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlType;

@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "", propOrder = {
        "category"
})
public class Categories {

    @XmlElement(required = true)
    public Category category;
}