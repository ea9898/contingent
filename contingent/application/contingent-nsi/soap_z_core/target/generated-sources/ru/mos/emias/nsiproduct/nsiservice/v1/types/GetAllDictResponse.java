
package ru.mos.emias.nsiproduct.nsiservice.v1.types;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import ru.mos.emias.nsiproduct.core.v1.EhdDictionaries;


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
 *         &lt;element name="ehdDictionaries" type="{http://emias.mos.ru/nsiProduct/core/v1/}ehdDictionaries"/&gt;
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
    "ehdDictionaries"
})
@XmlRootElement(name = "getAllDictResponse")
public class GetAllDictResponse {

    @XmlElement(required = true)
    protected EhdDictionaries ehdDictionaries;

    /**
     * Gets the value of the ehdDictionaries property.
     * 
     * @return
     *     possible object is
     *     {@link EhdDictionaries }
     *     
     */
    public EhdDictionaries getEhdDictionaries() {
        return ehdDictionaries;
    }

    /**
     * Sets the value of the ehdDictionaries property.
     * 
     * @param value
     *     allowed object is
     *     {@link EhdDictionaries }
     *     
     */
    public void setEhdDictionaries(EhdDictionaries value) {
        this.ehdDictionaries = value;
    }

}
