
package ru.mos.emias.nsiproduct.core.v1;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for item complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="item"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="data" type="{http://emias.mos.ru/nsiProduct/core/v1/}data"/&gt;
 *         &lt;element name="geoData" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *       &lt;/sequence&gt;
 *       &lt;attribute name="action" type="{http://www.w3.org/2001/XMLSchema}string" /&gt;
 *       &lt;attribute name="isManualGeo" type="{http://www.w3.org/2001/XMLSchema}boolean" /&gt;
 *       &lt;attribute name="systemObjectId" type="{http://www.w3.org/2001/XMLSchema}string" /&gt;
 *       &lt;attribute name="globalId" type="{http://www.w3.org/2001/XMLSchema}int" /&gt;
 *       &lt;attribute name="catalogId" type="{http://www.w3.org/2001/XMLSchema}int" /&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "item", propOrder = {
    "data",
    "geoData"
})
public class Item {

    @XmlElement(required = true)
    protected Data data;
    @XmlElement(required = true)
    protected String geoData;
    @XmlAttribute(name = "action")
    protected String action;
    @XmlAttribute(name = "isManualGeo")
    protected Boolean isManualGeo;
    @XmlAttribute(name = "systemObjectId")
    protected String systemObjectId;
    @XmlAttribute(name = "globalId")
    protected Integer globalId;
    @XmlAttribute(name = "catalogId")
    protected Integer catalogId;

    /**
     * Gets the value of the data property.
     * 
     * @return
     *     possible object is
     *     {@link Data }
     *     
     */
    public Data getData() {
        return data;
    }

    /**
     * Sets the value of the data property.
     * 
     * @param value
     *     allowed object is
     *     {@link Data }
     *     
     */
    public void setData(Data value) {
        this.data = value;
    }

    /**
     * Gets the value of the geoData property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getGeoData() {
        return geoData;
    }

    /**
     * Sets the value of the geoData property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setGeoData(String value) {
        this.geoData = value;
    }

    /**
     * Gets the value of the action property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getAction() {
        return action;
    }

    /**
     * Sets the value of the action property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setAction(String value) {
        this.action = value;
    }

    /**
     * Gets the value of the isManualGeo property.
     * 
     * @return
     *     possible object is
     *     {@link Boolean }
     *     
     */
    public Boolean isIsManualGeo() {
        return isManualGeo;
    }

    /**
     * Sets the value of the isManualGeo property.
     * 
     * @param value
     *     allowed object is
     *     {@link Boolean }
     *     
     */
    public void setIsManualGeo(Boolean value) {
        this.isManualGeo = value;
    }

    /**
     * Gets the value of the systemObjectId property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getSystemObjectId() {
        return systemObjectId;
    }

    /**
     * Sets the value of the systemObjectId property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setSystemObjectId(String value) {
        this.systemObjectId = value;
    }

    /**
     * Gets the value of the globalId property.
     * 
     * @return
     *     possible object is
     *     {@link Integer }
     *     
     */
    public Integer getGlobalId() {
        return globalId;
    }

    /**
     * Sets the value of the globalId property.
     * 
     * @param value
     *     allowed object is
     *     {@link Integer }
     *     
     */
    public void setGlobalId(Integer value) {
        this.globalId = value;
    }

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

}
