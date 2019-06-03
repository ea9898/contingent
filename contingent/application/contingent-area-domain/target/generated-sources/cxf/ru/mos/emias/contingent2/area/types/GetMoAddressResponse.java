
package ru.mos.emias.contingent2.area.types;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import ru.mos.emias.contingent2.core.AddressAllocationOrder;
import ru.mos.emias.contingent2.core.AreaTypeShort;
import ru.mos.emias.contingent2.core.MoAddress;
import ru.mos.emias.contingent2.core.PagingResults;


/**
 * <p>Java class for anonymous complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType&gt;
 *   &lt;complexContent&gt;
 *     &lt;extension base="{http://emias.mos.ru/contingent2/core/v1/}PagingResults"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="moId" type="{http://www.w3.org/2001/XMLSchema}long"/&gt;
 *         &lt;element name="areaType" type="{http://emias.mos.ru/contingent2/core/v1/}AreaTypeShort" maxOccurs="unbounded" minOccurs="0"/&gt;
 *         &lt;element name="order" type="{http://emias.mos.ru/contingent2/core/v1/}AddressAllocationOrder" maxOccurs="unbounded" minOccurs="0"/&gt;
 *         &lt;element name="moAddress" type="{http://emias.mos.ru/contingent2/core/v1/}MoAddress" maxOccurs="unbounded" minOccurs="0"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/extension&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "", propOrder = {
    "moId",
    "areaTypes",
    "orders",
    "moAddresses"
})
@XmlRootElement(name = "getMoAddressResponse")
public class GetMoAddressResponse
    extends PagingResults
    implements Serializable
{

    private final static long serialVersionUID = 1234567890L;
    protected long moId;
    @XmlElement(name = "areaType")
    protected List<AreaTypeShort> areaTypes;
    @XmlElement(name = "order")
    protected List<AddressAllocationOrder> orders;
    @XmlElement(name = "moAddress")
    protected List<MoAddress> moAddresses;

    /**
     * Gets the value of the moId property.
     * 
     */
    public long getMoId() {
        return moId;
    }

    /**
     * Sets the value of the moId property.
     * 
     */
    public void setMoId(long value) {
        this.moId = value;
    }

    /**
     * Gets the value of the areaTypes property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the areaTypes property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getAreaTypes().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link AreaTypeShort }
     * 
     * 
     */
    public List<AreaTypeShort> getAreaTypes() {
        if (areaTypes == null) {
            areaTypes = new ArrayList<AreaTypeShort>();
        }
        return this.areaTypes;
    }

    /**
     * Gets the value of the orders property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the orders property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getOrders().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link AddressAllocationOrder }
     * 
     * 
     */
    public List<AddressAllocationOrder> getOrders() {
        if (orders == null) {
            orders = new ArrayList<AddressAllocationOrder>();
        }
        return this.orders;
    }

    /**
     * Gets the value of the moAddresses property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the moAddresses property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getMoAddresses().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link MoAddress }
     * 
     * 
     */
    public List<MoAddress> getMoAddresses() {
        if (moAddresses == null) {
            moAddresses = new ArrayList<MoAddress>();
        }
        return this.moAddresses;
    }

}
