
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
 *         &lt;element name="muId" type="{http://www.w3.org/2001/XMLSchema}long"/&gt;
 *         &lt;element name="areaTypeCode" type="{http://www.w3.org/2001/XMLSchema}long" maxOccurs="unbounded"/&gt;
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
    "muId",
    "areaTypeCodes",
    "options"
})
@XmlRootElement(name = "delProfileMURequest")
public class DelProfileMURequest
    implements Serializable
{

    private final static long serialVersionUID = 1234567890L;
    protected long muId;
    @XmlElement(name = "areaTypeCode", type = Long.class)
    protected List<Long> areaTypeCodes;
    @XmlElement(namespace = "http://emias.mos.ru/contingent2/core/v1/")
    protected Options options;

    /**
     * Gets the value of the muId property.
     * 
     */
    public long getMuId() {
        return muId;
    }

    /**
     * Sets the value of the muId property.
     * 
     */
    public void setMuId(long value) {
        this.muId = value;
    }

    /**
     * Gets the value of the areaTypeCodes property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the areaTypeCodes property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getAreaTypeCodes().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link Long }
     * 
     * 
     */
    public List<Long> getAreaTypeCodes() {
        if (areaTypeCodes == null) {
            areaTypeCodes = new ArrayList<Long>();
        }
        return this.areaTypeCodes;
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
