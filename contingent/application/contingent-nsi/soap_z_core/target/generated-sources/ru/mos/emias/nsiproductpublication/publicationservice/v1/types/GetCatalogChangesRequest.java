
package ru.mos.emias.nsiproductpublication.publicationservice.v1.types;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for anonymous complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="offset" type="{http://www.w3.org/2001/XMLSchema}int"/&gt;
 *         &lt;element name="limit" type="{http://www.w3.org/2001/XMLSchema}int"/&gt;
 *         &lt;element name="id" type="{http://www.w3.org/2001/XMLSchema}int"/&gt;
 *         &lt;element name="criteria" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="sorting" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="from" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="until" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="state" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="translated" type="{http://www.w3.org/2001/XMLSchema}boolean"/&gt;
 *         &lt;element name="fetchGeodata" type="{http://www.w3.org/2001/XMLSchema}boolean"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "", propOrder = {
    "offset",
    "limit",
    "id",
    "criteria",
    "sorting",
    "from",
    "until",
    "state",
    "translated",
    "fetchGeodata"
})
@XmlRootElement(name = "getCatalogChangesRequest")
public class GetCatalogChangesRequest {

    @XmlElement(required = true, type = Integer.class, defaultValue = "0", nillable = true)
    protected Integer offset;
    @XmlElement(required = true, type = Integer.class, defaultValue = "1", nillable = true)
    protected Integer limit;
    protected int id;
    @XmlElement(required = true)
    protected String criteria;
    @XmlElement(required = true)
    protected String sorting;
    @XmlElement(required = true)
    protected String from;
    @XmlElement(required = true)
    protected String until;
    @XmlElement(required = true)
    protected String state;
    protected boolean translated;
    protected boolean fetchGeodata;

    /**
     * Gets the value of the offset property.
     * 
     * @return
     *     possible object is
     *     {@link Integer }
     *     
     */
    public Integer getOffset() {
        return offset;
    }

    /**
     * Sets the value of the offset property.
     * 
     * @param value
     *     allowed object is
     *     {@link Integer }
     *     
     */
    public void setOffset(Integer value) {
        this.offset = value;
    }

    /**
     * Gets the value of the limit property.
     * 
     * @return
     *     possible object is
     *     {@link Integer }
     *     
     */
    public Integer getLimit() {
        return limit;
    }

    /**
     * Sets the value of the limit property.
     * 
     * @param value
     *     allowed object is
     *     {@link Integer }
     *     
     */
    public void setLimit(Integer value) {
        this.limit = value;
    }

    /**
     * Gets the value of the id property.
     * 
     */
    public int getId() {
        return id;
    }

    /**
     * Sets the value of the id property.
     * 
     */
    public void setId(int value) {
        this.id = value;
    }

    /**
     * Gets the value of the criteria property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getCriteria() {
        return criteria;
    }

    /**
     * Sets the value of the criteria property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCriteria(String value) {
        this.criteria = value;
    }

    /**
     * Gets the value of the sorting property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getSorting() {
        return sorting;
    }

    /**
     * Sets the value of the sorting property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setSorting(String value) {
        this.sorting = value;
    }

    /**
     * Gets the value of the from property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getFrom() {
        return from;
    }

    /**
     * Sets the value of the from property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setFrom(String value) {
        this.from = value;
    }

    /**
     * Gets the value of the until property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getUntil() {
        return until;
    }

    /**
     * Sets the value of the until property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setUntil(String value) {
        this.until = value;
    }

    /**
     * Gets the value of the state property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getState() {
        return state;
    }

    /**
     * Sets the value of the state property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setState(String value) {
        this.state = value;
    }

    /**
     * Gets the value of the translated property.
     * 
     */
    public boolean isTranslated() {
        return translated;
    }

    /**
     * Sets the value of the translated property.
     * 
     */
    public void setTranslated(boolean value) {
        this.translated = value;
    }

    /**
     * Gets the value of the fetchGeodata property.
     * 
     */
    public boolean isFetchGeodata() {
        return fetchGeodata;
    }

    /**
     * Sets the value of the fetchGeodata property.
     * 
     */
    public void setFetchGeodata(boolean value) {
        this.fetchGeodata = value;
    }

}
