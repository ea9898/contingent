package moscow.ptnl.contingent.nsi.pushaccepter.xmlparsingS;

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "")
@XmlRootElement(name = "table")
public class Table {
    public Integer id;
    public String fullName;
    public String technicalName;
    public String shortName;
    public String accountingObject;
    public String keywords;
    public String vid;
    public String type;
    public String period;
    public String hasGeo;
    public String categories;
    public String oiv;
    public Integer packageId;//Todo сказать Соне добавить!
    public Attributes attributes;
}
