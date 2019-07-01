
package ru.mos.emias.nsiproductpublication.publicationservice.v1.types;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import ru.mos.emias.nsiproductpublication.core.v1.EhdDictionaryItems;


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
 *         &lt;element name="dictionaryItems" type="{http://emias.mos.ru/nsiProductPublication/core/v1/}ehdDictionaryItems"/&gt;
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
    "dictionaryItems"
})
@XmlRootElement(name = "getDictionaryItemResponse")
public class GetDictionaryItemResponse {

    @XmlElement(required = true)
    protected EhdDictionaryItems dictionaryItems;

    /**
     * Gets the value of the dictionaryItems property.
     * 
     * @return
     *     possible object is
     *     {@link EhdDictionaryItems }
     *     
     */
    public EhdDictionaryItems getDictionaryItems() {
        return dictionaryItems;
    }

    /**
     * Sets the value of the dictionaryItems property.
     * 
     * @param value
     *     allowed object is
     *     {@link EhdDictionaryItems }
     *     
     */
    public void setDictionaryItems(EhdDictionaryItems value) {
        this.dictionaryItems = value;
    }

}
