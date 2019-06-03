
package ru.mos.emias.contingent2.area.types;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import ru.mos.emias.contingent2.core.NotNsiAddress;
import ru.mos.emias.contingent2.core.NsiAddress;
import ru.mos.emias.contingent2.core.Options;


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
 *         &lt;element name="areaId" type="{http://www.w3.org/2001/XMLSchema}long"/&gt;
 *         &lt;element name="nsiAddress" type="{http://emias.mos.ru/contingent2/core/v1/}NsiAddress" maxOccurs="unbounded" minOccurs="0"/&gt;
 *         &lt;element name="nonNsiAddress" type="{http://emias.mos.ru/contingent2/core/v1/}NotNsiAddress" maxOccurs="unbounded" minOccurs="0"/&gt;
 *         &lt;element ref="{http://emias.mos.ru/contingent2/core/v1/}options" minOccurs="0"/&gt;
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
    "areaId",
    "nsiAddresses",
    "nonNsiAddresses",
    "options"
})
@XmlRootElement(name = "addAreaAddressRequest")
public class AddAreaAddressRequest
    implements Serializable
{

    private final static long serialVersionUID = 1234567890L;
    protected long areaId;
    @XmlElement(name = "nsiAddress")
    protected List<NsiAddress> nsiAddresses;
    @XmlElement(name = "nonNsiAddress")
    protected List<NotNsiAddress> nonNsiAddresses;
    @XmlElement(namespace = "http://emias.mos.ru/contingent2/core/v1/")
    protected Options options;

    /**
     * Gets the value of the areaId property.
     * 
     */
    public long getAreaId() {
        return areaId;
    }

    /**
     * Sets the value of the areaId property.
     * 
     */
    public void setAreaId(long value) {
        this.areaId = value;
    }

    /**
     * Gets the value of the nsiAddresses property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the nsiAddresses property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getNsiAddresses().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link NsiAddress }
     * 
     * 
     */
    public List<NsiAddress> getNsiAddresses() {
        if (nsiAddresses == null) {
            nsiAddresses = new ArrayList<NsiAddress>();
        }
        return this.nsiAddresses;
    }

    /**
     * Gets the value of the nonNsiAddresses property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the nonNsiAddresses property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getNonNsiAddresses().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link NotNsiAddress }
     * 
     * 
     */
    public List<NotNsiAddress> getNonNsiAddresses() {
        if (nonNsiAddresses == null) {
            nonNsiAddresses = new ArrayList<NotNsiAddress>();
        }
        return this.nonNsiAddresses;
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

}
