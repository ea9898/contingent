
package ru.mos.emias.nsiproduct.nsiservice.v1.types;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import ru.mos.emias.nsiproduct.core.v1.EhdDictionaryItems;
import ru.mos.emias.nsiproduct.core.v1.PagingResults;


/**
 * <p>Java class for anonymous complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType&gt;
 *   &lt;complexContent&gt;
 *     &lt;extension base="{http://emias.mos.ru/nsiProduct/core/v1/}PagingResults"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="ehdDictionaryItems" type="{http://emias.mos.ru/nsiProduct/core/v1/}ehdDictionaryItems"/&gt;
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
    "ehdDictionaryItems"
})
@XmlRootElement(name = "getDictItemResponse")
public class GetDictItemResponse
    extends PagingResults
{

    @XmlElement(required = true)
    protected EhdDictionaryItems ehdDictionaryItems;

    /**
     * Gets the value of the ehdDictionaryItems property.
     * 
     * @return
     *     possible object is
     *     {@link EhdDictionaryItems }
     *     
     */
    public EhdDictionaryItems getEhdDictionaryItems() {
        return ehdDictionaryItems;
    }

    /**
     * Sets the value of the ehdDictionaryItems property.
     * 
     * @param value
     *     allowed object is
     *     {@link EhdDictionaryItems }
     *     
     */
    public void setEhdDictionaryItems(EhdDictionaryItems value) {
        this.ehdDictionaryItems = value;
    }

}
