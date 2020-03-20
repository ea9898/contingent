package moscow.ptnl.contingent.nsi.pushaccepter.xmlparsingS;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

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
