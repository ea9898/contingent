
package ru.mos.emias.contingent2.area.v2.types;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import ru.mos.emias.contingent2.core.v2.Options;


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
 *         &lt;element name="options" type="{http://emias.mos.ru/contingent2/core/v2/}Options" minOccurs="0"/&gt;
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
    "options"
})
@XmlRootElement(name = "getProfileMURequest")
public class GetProfileMURequest
    implements Serializable
{

    private final static long serialVersionUID = 1234567890L;
    protected long muId;
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
