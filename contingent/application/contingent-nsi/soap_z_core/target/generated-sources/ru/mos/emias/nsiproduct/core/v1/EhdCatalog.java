
package ru.mos.emias.nsiproduct.core.v1;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for ehdCatalog complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="ehdCatalog"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="id" type="{http://www.w3.org/2001/XMLSchema}long"/&gt;
 *         &lt;element name="fullName" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="technicalName" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="shortName" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="accountingObject" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="keywords" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="vid" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="type" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="period" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="hasGeo" type="{http://www.w3.org/2001/XMLSchema}boolean"/&gt;
 *         &lt;element name="categories" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="oiv" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "ehdCatalog", propOrder = {
    "id",
    "fullName",
    "technicalName",
    "shortName",
    "accountingObject",
    "keywords",
    "vid",
    "type",
    "period",
    "hasGeo",
    "categories",
    "oiv"
})
public class EhdCatalog {

    @XmlElement(required = true, type = Long.class, nillable = true)
    protected Long id;
    @XmlElement(required = true)
    protected String fullName;
    @XmlElement(required = true)
    protected String technicalName;
    @XmlElement(required = true)
    protected String shortName;
    @XmlElement(required = true)
    protected String accountingObject;
    @XmlElement(required = true)
    protected String keywords;
    @XmlElement(required = true)
    protected String vid;
    @XmlElement(required = true)
    protected String type;
    @XmlElement(required = true)
    protected String period;
    @XmlElement(required = true, type = Boolean.class, nillable = true)
    protected Boolean hasGeo;
    @XmlElement(required = true)
    protected String categories;
    @XmlElement(required = true)
    protected String oiv;

    /**
     * Gets the value of the id property.
     * 
     * @return
     *     possible object is
     *     {@link Long }
     *     
     */
    public Long getId() {
        return id;
    }

    /**
     * Sets the value of the id property.
     * 
     * @param value
     *     allowed object is
     *     {@link Long }
     *     
     */
    public void setId(Long value) {
        this.id = value;
    }

    /**
     * Gets the value of the fullName property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getFullName() {
        return fullName;
    }

    /**
     * Sets the value of the fullName property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setFullName(String value) {
        this.fullName = value;
    }

    /**
     * Gets the value of the technicalName property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getTechnicalName() {
        return technicalName;
    }

    /**
     * Sets the value of the technicalName property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setTechnicalName(String value) {
        this.technicalName = value;
    }

    /**
     * Gets the value of the shortName property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getShortName() {
        return shortName;
    }

    /**
     * Sets the value of the shortName property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setShortName(String value) {
        this.shortName = value;
    }

    /**
     * Gets the value of the accountingObject property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getAccountingObject() {
        return accountingObject;
    }

    /**
     * Sets the value of the accountingObject property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setAccountingObject(String value) {
        this.accountingObject = value;
    }

    /**
     * Gets the value of the keywords property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getKeywords() {
        return keywords;
    }

    /**
     * Sets the value of the keywords property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setKeywords(String value) {
        this.keywords = value;
    }

    /**
     * Gets the value of the vid property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getVid() {
        return vid;
    }

    /**
     * Sets the value of the vid property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setVid(String value) {
        this.vid = value;
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
     * Gets the value of the period property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getPeriod() {
        return period;
    }

    /**
     * Sets the value of the period property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setPeriod(String value) {
        this.period = value;
    }

    /**
     * Gets the value of the hasGeo property.
     * 
     * @return
     *     possible object is
     *     {@link Boolean }
     *     
     */
    public Boolean isHasGeo() {
        return hasGeo;
    }

    /**
     * Sets the value of the hasGeo property.
     * 
     * @param value
     *     allowed object is
     *     {@link Boolean }
     *     
     */
    public void setHasGeo(Boolean value) {
        this.hasGeo = value;
    }

    /**
     * Gets the value of the categories property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getCategories() {
        return categories;
    }

    /**
     * Sets the value of the categories property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCategories(String value) {
        this.categories = value;
    }

    /**
     * Gets the value of the oiv property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getOiv() {
        return oiv;
    }

    /**
     * Sets the value of the oiv property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setOiv(String value) {
        this.oiv = value;
    }

}
