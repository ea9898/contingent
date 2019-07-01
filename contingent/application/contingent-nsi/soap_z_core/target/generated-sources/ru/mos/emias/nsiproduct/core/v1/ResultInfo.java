
package ru.mos.emias.nsiproduct.core.v1;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for resultInfo complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="resultInfo"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="status" type="{http://www.w3.org/2001/XMLSchema}int"/&gt;
 *         &lt;element name="message" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="detailMessage" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="globalId" type="{http://www.w3.org/2001/XMLSchema}long"/&gt;
 *         &lt;element name="systemObjectId" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="action" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "resultInfo", propOrder = {
    "status",
    "message",
    "detailMessage",
    "globalId",
    "systemObjectId",
    "action"
})
public class ResultInfo {

    @XmlElement(required = true, type = Integer.class, nillable = true)
    protected Integer status;
    @XmlElement(required = true)
    protected String message;
    @XmlElement(required = true)
    protected String detailMessage;
    @XmlElement(required = true, type = Long.class, nillable = true)
    protected Long globalId;
    @XmlElement(required = true)
    protected String systemObjectId;
    @XmlElement(required = true)
    protected String action;

    /**
     * Gets the value of the status property.
     * 
     * @return
     *     possible object is
     *     {@link Integer }
     *     
     */
    public Integer getStatus() {
        return status;
    }

    /**
     * Sets the value of the status property.
     * 
     * @param value
     *     allowed object is
     *     {@link Integer }
     *     
     */
    public void setStatus(Integer value) {
        this.status = value;
    }

    /**
     * Gets the value of the message property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getMessage() {
        return message;
    }

    /**
     * Sets the value of the message property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setMessage(String value) {
        this.message = value;
    }

    /**
     * Gets the value of the detailMessage property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getDetailMessage() {
        return detailMessage;
    }

    /**
     * Sets the value of the detailMessage property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setDetailMessage(String value) {
        this.detailMessage = value;
    }

    /**
     * Gets the value of the globalId property.
     * 
     * @return
     *     possible object is
     *     {@link Long }
     *     
     */
    public Long getGlobalId() {
        return globalId;
    }

    /**
     * Sets the value of the globalId property.
     * 
     * @param value
     *     allowed object is
     *     {@link Long }
     *     
     */
    public void setGlobalId(Long value) {
        this.globalId = value;
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

}
