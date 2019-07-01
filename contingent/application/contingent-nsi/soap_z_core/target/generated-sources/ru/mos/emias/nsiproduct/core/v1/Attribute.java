
package ru.mos.emias.nsiproduct.core.v1;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for attribute complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="attribute"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="values" type="{http://emias.mos.ru/nsiProduct/core/v1/}values"/&gt;
 *         &lt;element name="files" type="{http://emias.mos.ru/nsiProduct/core/v1/}files"/&gt;
 *         &lt;element name="items" type="{http://emias.mos.ru/nsiProduct/core/v1/}items"/&gt;
 *       &lt;/sequence&gt;
 *       &lt;attribute name="tehName" type="{http://www.w3.org/2001/XMLSchema}string" /&gt;
 *       &lt;attribute name="isManual" type="{http://www.w3.org/2001/XMLSchema}boolean" default="false" /&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "attribute", propOrder = {
    "values",
    "files",
    "items"
})
public class Attribute {

    @XmlElement(required = true, nillable = true)
    protected Values values;
    @XmlElement(required = true, nillable = true)
    protected Files files;
    @XmlElement(required = true, nillable = true)
    protected Items items;
    @XmlAttribute(name = "tehName")
    protected String tehName;
    @XmlAttribute(name = "isManual")
    protected Boolean isManual;

    /**
     * Gets the value of the values property.
     * 
     * @return
     *     possible object is
     *     {@link Values }
     *     
     */
    public Values getValues() {
        return values;
    }

    /**
     * Sets the value of the values property.
     * 
     * @param value
     *     allowed object is
     *     {@link Values }
     *     
     */
    public void setValues(Values value) {
        this.values = value;
    }

    /**
     * Gets the value of the files property.
     * 
     * @return
     *     possible object is
     *     {@link Files }
     *     
     */
    public Files getFiles() {
        return files;
    }

    /**
     * Sets the value of the files property.
     * 
     * @param value
     *     allowed object is
     *     {@link Files }
     *     
     */
    public void setFiles(Files value) {
        this.files = value;
    }

    /**
     * Gets the value of the items property.
     * 
     * @return
     *     possible object is
     *     {@link Items }
     *     
     */
    public Items getItems() {
        return items;
    }

    /**
     * Sets the value of the items property.
     * 
     * @param value
     *     allowed object is
     *     {@link Items }
     *     
     */
    public void setItems(Items value) {
        this.items = value;
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
     * Gets the value of the isManual property.
     * 
     * @return
     *     possible object is
     *     {@link Boolean }
     *     
     */
    public boolean isIsManual() {
        if (isManual == null) {
            return false;
        } else {
            return isManual;
        }
    }

    /**
     * Sets the value of the isManual property.
     * 
     * @param value
     *     allowed object is
     *     {@link Boolean }
     *     
     */
    public void setIsManual(Boolean value) {
        this.isManual = value;
    }

}
