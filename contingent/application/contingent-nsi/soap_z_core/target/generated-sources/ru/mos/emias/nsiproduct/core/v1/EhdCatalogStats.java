
package ru.mos.emias.nsiproduct.core.v1;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for ehdCatalogStats complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="ehdCatalogStats"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="catalogId" type="{http://www.w3.org/2001/XMLSchema}int"/&gt;
 *         &lt;element name="fullName" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="technicalName" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="cntActiveObj" type="{http://www.w3.org/2001/XMLSchema}int"/&gt;
 *         &lt;element name="cntDelObj" type="{http://www.w3.org/2001/XMLSchema}int"/&gt;
 *         &lt;element name="cntNotSubscribe" type="{http://www.w3.org/2001/XMLSchema}int"/&gt;
 *         &lt;element name="cntError" type="{http://www.w3.org/2001/XMLSchema}int"/&gt;
 *         &lt;element name="cntGeoError" type="{http://www.w3.org/2001/XMLSchema}int"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "ehdCatalogStats", propOrder = {
    "catalogId",
    "fullName",
    "technicalName",
    "cntActiveObj",
    "cntDelObj",
    "cntNotSubscribe",
    "cntError",
    "cntGeoError"
})
public class EhdCatalogStats {

    @XmlElement(required = true, type = Integer.class, nillable = true)
    protected Integer catalogId;
    @XmlElement(required = true, nillable = true)
    protected String fullName;
    @XmlElement(required = true)
    protected String technicalName;
    @XmlElement(required = true, type = Integer.class, nillable = true)
    protected Integer cntActiveObj;
    @XmlElement(required = true, type = Integer.class, nillable = true)
    protected Integer cntDelObj;
    @XmlElement(required = true, type = Integer.class, nillable = true)
    protected Integer cntNotSubscribe;
    @XmlElement(required = true, type = Integer.class, nillable = true)
    protected Integer cntError;
    @XmlElement(required = true, type = Integer.class, nillable = true)
    protected Integer cntGeoError;

    /**
     * Gets the value of the catalogId property.
     * 
     * @return
     *     possible object is
     *     {@link Integer }
     *     
     */
    public Integer getCatalogId() {
        return catalogId;
    }

    /**
     * Sets the value of the catalogId property.
     * 
     * @param value
     *     allowed object is
     *     {@link Integer }
     *     
     */
    public void setCatalogId(Integer value) {
        this.catalogId = value;
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
     * Gets the value of the cntActiveObj property.
     * 
     * @return
     *     possible object is
     *     {@link Integer }
     *     
     */
    public Integer getCntActiveObj() {
        return cntActiveObj;
    }

    /**
     * Sets the value of the cntActiveObj property.
     * 
     * @param value
     *     allowed object is
     *     {@link Integer }
     *     
     */
    public void setCntActiveObj(Integer value) {
        this.cntActiveObj = value;
    }

    /**
     * Gets the value of the cntDelObj property.
     * 
     * @return
     *     possible object is
     *     {@link Integer }
     *     
     */
    public Integer getCntDelObj() {
        return cntDelObj;
    }

    /**
     * Sets the value of the cntDelObj property.
     * 
     * @param value
     *     allowed object is
     *     {@link Integer }
     *     
     */
    public void setCntDelObj(Integer value) {
        this.cntDelObj = value;
    }

    /**
     * Gets the value of the cntNotSubscribe property.
     * 
     * @return
     *     possible object is
     *     {@link Integer }
     *     
     */
    public Integer getCntNotSubscribe() {
        return cntNotSubscribe;
    }

    /**
     * Sets the value of the cntNotSubscribe property.
     * 
     * @param value
     *     allowed object is
     *     {@link Integer }
     *     
     */
    public void setCntNotSubscribe(Integer value) {
        this.cntNotSubscribe = value;
    }

    /**
     * Gets the value of the cntError property.
     * 
     * @return
     *     possible object is
     *     {@link Integer }
     *     
     */
    public Integer getCntError() {
        return cntError;
    }

    /**
     * Sets the value of the cntError property.
     * 
     * @param value
     *     allowed object is
     *     {@link Integer }
     *     
     */
    public void setCntError(Integer value) {
        this.cntError = value;
    }

    /**
     * Gets the value of the cntGeoError property.
     * 
     * @return
     *     possible object is
     *     {@link Integer }
     *     
     */
    public Integer getCntGeoError() {
        return cntGeoError;
    }

    /**
     * Sets the value of the cntGeoError property.
     * 
     * @param value
     *     allowed object is
     *     {@link Integer }
     *     
     */
    public void setCntGeoError(Integer value) {
        this.cntGeoError = value;
    }

}
