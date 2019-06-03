
package ru.mos.emias.contingent2.core;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for MoAddress complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="MoAddress"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="moAddressId" type="{http://www.w3.org/2001/XMLSchema}long"/&gt;
 *         &lt;element name="areaTypeCode" type="{http://www.w3.org/2001/XMLSchema}long"/&gt;
 *         &lt;element name="orderId" type="{http://www.w3.org/2001/XMLSchema}long"/&gt;
 *         &lt;element name="address" type="{http://emias.mos.ru/contingent2/core/v1/}Address"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "MoAddress", propOrder = {
    "moAddressId",
    "areaTypeCode",
    "orderId",
    "address"
})
public class MoAddress
    implements Serializable
{

    private final static long serialVersionUID = 1234567890L;
    protected long moAddressId;
    protected long areaTypeCode;
    protected long orderId;
    @XmlElement(required = true)
    protected Address address;

    /**
     * Gets the value of the moAddressId property.
     * 
     */
    public long getMoAddressId() {
        return moAddressId;
    }

    /**
     * Sets the value of the moAddressId property.
     * 
     */
    public void setMoAddressId(long value) {
        this.moAddressId = value;
    }

    /**
     * Gets the value of the areaTypeCode property.
     * 
     */
    public long getAreaTypeCode() {
        return areaTypeCode;
    }

    /**
     * Sets the value of the areaTypeCode property.
     * 
     */
    public void setAreaTypeCode(long value) {
        this.areaTypeCode = value;
    }

    /**
     * Gets the value of the orderId property.
     * 
     */
    public long getOrderId() {
        return orderId;
    }

    /**
     * Sets the value of the orderId property.
     * 
     */
    public void setOrderId(long value) {
        this.orderId = value;
    }

    /**
     * Gets the value of the address property.
     * 
     * @return
     *     possible object is
     *     {@link Address }
     *     
     */
    public Address getAddress() {
        return address;
    }

    /**
     * Sets the value of the address property.
     * 
     * @param value
     *     allowed object is
     *     {@link Address }
     *     
     */
    public void setAddress(Address value) {
        this.address = value;
    }

}
