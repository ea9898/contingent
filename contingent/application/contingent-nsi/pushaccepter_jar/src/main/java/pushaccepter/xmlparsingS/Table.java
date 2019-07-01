package pushaccepter.xmlparsingS;

import pushaccepter.xmlparsing.Values;

import javax.xml.bind.annotation.*;

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
