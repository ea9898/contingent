
package ru.mos.emias.contingent2.area.types;

import java.io.Serializable;
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
 *         &lt;element name="muTypeId" type="{http://www.w3.org/2001/XMLSchema}long"/&gt;
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
    "muTypeId",
    "options"
})
@XmlRootElement(name = "getProfileMURequest")
public class GetProfileMURequest
    implements Serializable
{

    private final static long serialVersionUID = 1234567890L;
    protected long muId;
    protected long muTypeId;
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
     * Gets the value of the muTypeId property.
     * 
     */
    public long getMuTypeId() {
        return muTypeId;
    }

    /**
     * Sets the value of the muTypeId property.
     * 
     */
    public void setMuTypeId(long value) {
        this.muTypeId = value;
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
