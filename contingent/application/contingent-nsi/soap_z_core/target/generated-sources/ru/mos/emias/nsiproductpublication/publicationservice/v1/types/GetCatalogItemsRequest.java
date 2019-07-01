
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
 *         &lt;element name="projection" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="sorting" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="useAntiProjection" type="{http://www.w3.org/2001/XMLSchema}boolean"/&gt;
 *         &lt;element name="includeDeleted" type="{http://www.w3.org/2001/XMLSchema}boolean"/&gt;
 *         &lt;element name="fetchGeodata" type="{http://www.w3.org/2001/XMLSchema}boolean"/&gt;
 *         &lt;element name="epoch" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="sortByPolygon" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="timestamp" type="{http://www.w3.org/2001/XMLSchema}int"/&gt;
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
    "projection",
    "sorting",
    "useAntiProjection",
    "includeDeleted",
    "fetchGeodata",
    "epoch",
    "sortByPolygon",
    "timestamp"
})
@XmlRootElement(name = "getCatalogItemsRequest")
public class GetCatalogItemsRequest {

    @XmlElement(required = true, type = Integer.class, defaultValue = "0", nillable = true)
    protected Integer offset;
    @XmlElement(required = true, type = Integer.class, defaultValue = "1", nillable = true)
    protected Integer limit;
    protected int id;
    @XmlElement(required = true)
    protected String criteria;
    @XmlElement(required = true)
    protected String projection;
    @XmlElement(required = true)
    protected String sorting;
    protected boolean useAntiProjection;
    protected boolean includeDeleted;
    protected boolean fetchGeodata;
    @XmlElement(required = true, nillable = true)
    protected String epoch;
    @XmlElement(required = true, nillable = true)
    protected String sortByPolygon;
    @XmlElement(required = true, type = Integer.class, nillable = true)
    protected Integer timestamp;

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
     * Gets the value of the projection property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getProjection() {
        return projection;
    }

    /**
     * Sets the value of the projection property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setProjection(String value) {
        this.projection = value;
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
     * Gets the value of the useAntiProjection property.
     * 
     */
    public boolean isUseAntiProjection() {
        return useAntiProjection;
    }

    /**
     * Sets the value of the useAntiProjection property.
     * 
     */
    public void setUseAntiProjection(boolean value) {
        this.useAntiProjection = value;
    }

    /**
     * Gets the value of the includeDeleted property.
     * 
     */
    public boolean isIncludeDeleted() {
        return includeDeleted;
    }

    /**
     * Sets the value of the includeDeleted property.
     * 
     */
    public void setIncludeDeleted(boolean value) {
        this.includeDeleted = value;
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

    /**
     * Gets the value of the epoch property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getEpoch() {
        return epoch;
    }

    /**
     * Sets the value of the epoch property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setEpoch(String value) {
        this.epoch = value;
    }

    /**
     * Gets the value of the sortByPolygon property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getSortByPolygon() {
        return sortByPolygon;
    }

    /**
     * Sets the value of the sortByPolygon property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setSortByPolygon(String value) {
        this.sortByPolygon = value;
    }

    /**
     * Gets the value of the timestamp property.
     * 
     * @return
     *     possible object is
     *     {@link Integer }
     *     
     */
    public Integer getTimestamp() {
        return timestamp;
    }

    /**
     * Sets the value of the timestamp property.
     * 
     * @param value
     *     allowed object is
     *     {@link Integer }
     *     
     */
    public void setTimestamp(Integer value) {
        this.timestamp = value;
    }

}
