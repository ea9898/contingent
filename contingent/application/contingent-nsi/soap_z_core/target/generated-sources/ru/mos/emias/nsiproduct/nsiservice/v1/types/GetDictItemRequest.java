
package ru.mos.emias.nsiproduct.nsiservice.v1.types;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import ru.mos.emias.nsiproduct.core.v1.Options;
import ru.mos.emias.nsiproduct.core.v1.PagingOptions;


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
 *         &lt;element name="id" type="{http://www.w3.org/2001/XMLSchema}long"/&gt;
 *         &lt;element name="elementId" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element ref="{http://emias.mos.ru/nsiProduct/core/v1/}options" minOccurs="0"/&gt;
 *         &lt;element ref="{http://emias.mos.ru/nsiProduct/core/v1/}pagingOptions" minOccurs="0"/&gt;
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
    "id",
    "elementId",
    "options",
    "pagingOptions"
})
@XmlRootElement(name = "getDictItemRequest")
public class GetDictItemRequest {

    @XmlElement(required = true, type = Long.class, nillable = true)
    protected Long id;
    @XmlElement(required = true)
    protected String elementId;
    @XmlElement(namespace = "http://emias.mos.ru/nsiProduct/core/v1/")
    protected Options options;
    @XmlElement(namespace = "http://emias.mos.ru/nsiProduct/core/v1/")
    protected PagingOptions pagingOptions;

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
     * Gets the value of the elementId property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getElementId() {
        return elementId;
    }

    /**
     * Sets the value of the elementId property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setElementId(String value) {
        this.elementId = value;
    }

    /**
     * Gets the value of the options property.
     * 
     * @return
     *     possible object is
     *     {@link Options }
     *     
     */
    public Options getOptions() {
        return options;
    }

    /**
     * Sets the value of the options property.
     * 
     * @param value
     *     allowed object is
     *     {@link Options }
     *     
     */
    public void setOptions(Options value) {
        this.options = value;
    }

    /**
     * Gets the value of the pagingOptions property.
     * 
     * @return
     *     possible object is
     *     {@link PagingOptions }
     *     
     */
    public PagingOptions getPagingOptions() {
        return pagingOptions;
    }

    /**
     * Sets the value of the pagingOptions property.
     * 
     * @param value
     *     allowed object is
     *     {@link PagingOptions }
     *     
     */
    public void setPagingOptions(PagingOptions value) {
        this.pagingOptions = value;
    }

}
