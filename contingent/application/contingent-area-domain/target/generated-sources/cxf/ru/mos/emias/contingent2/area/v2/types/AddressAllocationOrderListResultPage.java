
package ru.mos.emias.contingent2.area.v2.types;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;
import ru.mos.emias.contingent2.core.v2.AddressAllocationOrder;
import ru.mos.emias.contingent2.core.v2.PagingResults;


/**
 * Результаты отбора сведений об участках
 * 
 * <p>Java class for AddressAllocationOrderListResultPage complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="AddressAllocationOrderListResultPage"&gt;
 *   &lt;complexContent&gt;
 *     &lt;extension base="{http://emias.mos.ru/contingent2/core/v2/}PagingResults"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="addressAllocationOrders" type="{http://emias.mos.ru/contingent2/core/v2/}AddressAllocationOrder" maxOccurs="unbounded" minOccurs="0"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/extension&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "AddressAllocationOrderListResultPage", propOrder = {
    "addressAllocationOrders"
})
public class AddressAllocationOrderListResultPage
    extends PagingResults
    implements Serializable
{

    private final static long serialVersionUID = 1234567890L;
    protected List<AddressAllocationOrder> addressAllocationOrders;

    /**
     * Gets the value of the addressAllocationOrders property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the addressAllocationOrders property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getAddressAllocationOrders().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link AddressAllocationOrder }
     * 
     * 
     */
    public List<AddressAllocationOrder> getAddressAllocationOrders() {
        if (addressAllocationOrders == null) {
            addressAllocationOrders = new ArrayList<AddressAllocationOrder>();
        }
        return this.addressAllocationOrders;
    }

}
