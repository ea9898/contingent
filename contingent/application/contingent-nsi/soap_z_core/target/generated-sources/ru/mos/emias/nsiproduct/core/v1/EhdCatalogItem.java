
package ru.mos.emias.nsiproduct.core.v1;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for ehdCatalogItem complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="ehdCatalogItem"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="id" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="isDeleted" type="{http://www.w3.org/2001/XMLSchema}int"/&gt;
 *         &lt;element name="isDeletedTmp" type="{http://www.w3.org/2001/XMLSchema}int"/&gt;
 *         &lt;element name="tehName" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="type" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="dictValue" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="value" type="{http://www.w3.org/2001/XMLSchema}anyType"/&gt;
 *         &lt;element name="groupValue" type="{http://emias.mos.ru/nsiProduct/core/v1/}ehdCatalogItems"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "ehdCatalogItem", propOrder = {
    "id",
    "isDeleted",
    "isDeletedTmp",
    "tehName",
    "type",
    "dictValue",
    "value",
    "groupValue"
})
public class EhdCatalogItem {

    @XmlElement(required = true)
    protected String id;
    @XmlElement(required = true, type = Integer.class, nillable = true)
    protected Integer isDeleted;
    @XmlElement(required = true, type = Integer.class, nillable = true)
    protected Integer isDeletedTmp;
    @XmlElement(required = true)
    protected String tehName;
    @XmlElement(required = true)
    protected String type;
    @XmlElement(required = true)
    protected String dictValue;
    @XmlElement(required = true)
    protected Object value;
    @XmlElement(required = true)
    protected EhdCatalogItems groupValue;

    /**
     * Gets the value of the id property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getId() {
        return id;
    }

    /**
     * Sets the value of the id property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setId(String value) {
        this.id = value;
    }

    /**
     * Gets the value of the isDeleted property.
     * 
     * @return
     *     possible object is
     *     {@link Integer }
     *     
     */
    public Integer getIsDeleted() {
        return isDeleted;
    }

    /**
     * Sets the value of the isDeleted property.
     * 
     * @param value
     *     allowed object is
     *     {@link Integer }
     *     
     */
    public void setIsDeleted(Integer value) {
        this.isDeleted = value;
    }

    /**
     * Gets the value of the isDeletedTmp property.
     * 
     * @return
     *     possible object is
     *     {@link Integer }
     *     
     */
    public Integer getIsDeletedTmp() {
        return isDeletedTmp;
    }

    /**
     * Sets the value of the isDeletedTmp property.
     * 
     * @param value
     *     allowed object is
     *     {@link Integer }
     *     
     */
    public void setIsDeletedTmp(Integer value) {
        this.isDeletedTmp = value;
    }

    /**
     * Gets the value of the tehName property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getTehName() {
        return tehName;
    }

    /**
     * Sets the value of the tehName property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setTehName(String value) {
        this.tehName = value;
    }

    /**
     * Gets the value of the type property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getType() {
        return type;
    }

    /**
     * Sets the value of the type property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setType(String value) {
        this.type = value;
    }

    /**
     * Gets the value of the dictValue property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getDictValue() {
        return dictValue;
    }

    /**
     * Sets the value of the dictValue property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setDictValue(String value) {
        this.dictValue = value;
    }

    /**
     * Gets the value of the value property.
     * 
     * @return
     *     possible object is
     *     {@link Object }
     *     
     */
    public Object getValue() {
        return value;
    }

    /**
     * Sets the value of the value property.
     * 
     * @param value
     *     allowed object is
     *     {@link Object }
     *     
     */
    public void setValue(Object value) {
        this.value = value;
    }

    /**
     * Gets the value of the groupValue property.
     * 
     * @return
     *     possible object is
     *     {@link EhdCatalogItems }
     *     
     */
    public EhdCatalogItems getGroupValue() {
        return groupValue;
    }

    /**
     * Sets the value of the groupValue property.
     * 
     * @param value
     *     allowed object is
     *     {@link EhdCatalogItems }
     *     
     */
    public void setGroupValue(EhdCatalogItems value) {
        this.groupValue = value;
    }

}
