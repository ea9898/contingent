
package ru.mos.emias.contingent2.area.types;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import ru.mos.emias.contingent2.core.Options;
import ru.mos.emias.contingent2.core.PagingOptions;


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
 *         &lt;element name="moId" type="{http://www.w3.org/2001/XMLSchema}long"/&gt;
 *         &lt;element name="areaType" type="{http://www.w3.org/2001/XMLSchema}long" maxOccurs="unbounded" minOccurs="0"/&gt;
 *         &lt;element ref="{http://emias.mos.ru/contingent2/core/v1/}options" minOccurs="0"/&gt;
 *         &lt;element ref="{http://emias.mos.ru/contingent2/core/v1/}pagingOptions" minOccurs="0"/&gt;
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
    "moId",
    "areaTypes",
    "options",
    "pagingOptions"
})
@XmlRootElement(name = "getMoAddressRequest")
public class GetMoAddressRequest
    implements Serializable
{

    private final static long serialVersionUID = 1234567890L;
    protected long moId;
    @XmlElement(name = "areaType", type = Long.class)
    protected List<Long> areaTypes;
    @XmlElement(namespace = "http://emias.mos.ru/contingent2/core/v1/")
    protected Options options;
    @XmlElement(namespace = "http://emias.mos.ru/contingent2/core/v1/")
    protected PagingOptions pagingOptions;

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
     * {@link Long }
     * 
     * 
     */
    public List<Long> getAreaTypes() {
        if (areaTypes == null) {
            areaTypes = new ArrayList<Long>();
        }
        return this.areaTypes;
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
