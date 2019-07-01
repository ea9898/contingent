
package ru.mos.emias.nsiproductpublication.core.v1;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for catalogExport complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="catalogExport"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element ref="{http://emias.mos.ru/nsiProductPublication/core/v1/}exportInfo" maxOccurs="unbounded"/&gt;
 *         &lt;element name="errors" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="structureVersion" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="createDate" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "catalogExport", propOrder = {
    "exportInfo",
    "errors",
    "structureVersion",
    "createDate"
})
public class CatalogExport {

    @XmlElement(required = true)
    protected List<ExportInfo> exportInfo;
    @XmlElement(required = true, nillable = true)
    protected String errors;
    @XmlElement(required = true, nillable = true)
    protected String structureVersion;
    @XmlElement(required = true)
    protected String createDate;

    /**
     * Gets the value of the exportInfo property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the exportInfo property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getExportInfo().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link ExportInfo }
     * 
     * 
     */
    public List<ExportInfo> getExportInfo() {
        if (exportInfo == null) {
            exportInfo = new ArrayList<ExportInfo>();
        }
        return this.exportInfo;
    }

    /**
     * Gets the value of the errors property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getErrors() {
        return errors;
    }

    /**
     * Sets the value of the errors property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setErrors(String value) {
        this.errors = value;
    }

    /**
     * Gets the value of the structureVersion property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getStructureVersion() {
        return structureVersion;
    }

    /**
     * Sets the value of the structureVersion property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setStructureVersion(String value) {
        this.structureVersion = value;
    }

    /**
     * Gets the value of the createDate property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getCreateDate() {
        return createDate;
    }

    /**
     * Sets the value of the createDate property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCreateDate(String value) {
        this.createDate = value;
    }

}
