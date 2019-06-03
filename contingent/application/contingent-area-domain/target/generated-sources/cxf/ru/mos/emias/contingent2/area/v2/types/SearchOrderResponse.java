
package ru.mos.emias.contingent2.area.v2.types;

import java.io.Serializable;
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
 *         &lt;element name="result" type="{http://emias.mos.ru/contingent2/area/v2/types/}AddressAllocationOrderListResultPage"/&gt;
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
    "result"
})
@XmlRootElement(name = "searchOrderResponse")
public class SearchOrderResponse
    implements Serializable
{

    private final static long serialVersionUID = 1234567890L;
    @XmlElement(required = true)
    protected AddressAllocationOrderListResultPage result;

    /**
     * Gets the value of the result property.
     * 
     * @return
     *     possible object is
     *     {@link AddressAllocationOrderListResultPage }
     *     
     */
    public AddressAllocationOrderListResultPage getResult() {
        return result;
    }

    /**
     * Sets the value of the result property.
     * 
     * @param value
     *     allowed object is
     *     {@link AddressAllocationOrderListResultPage }
     *     
     */
    public void setResult(AddressAllocationOrderListResultPage value) {
        this.result = value;
    }

}
