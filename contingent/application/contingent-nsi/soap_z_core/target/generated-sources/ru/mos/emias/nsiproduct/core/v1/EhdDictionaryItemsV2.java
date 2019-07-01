
package ru.mos.emias.nsiproduct.core.v1;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for ehdDictionaryItemsV2 complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="ehdDictionaryItemsV2"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="ehdDictionaryItemsV2" type="{http://emias.mos.ru/nsiProduct/core/v1/}ehdDictionaryItemV2" maxOccurs="unbounded" minOccurs="0"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "ehdDictionaryItemsV2", propOrder = {
    "ehdDictionaryItemsV2"
})
public class EhdDictionaryItemsV2 {

    protected List<EhdDictionaryItemV2> ehdDictionaryItemsV2;

    /**
     * Gets the value of the ehdDictionaryItemsV2 property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the ehdDictionaryItemsV2 property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getEhdDictionaryItemsV2().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link EhdDictionaryItemV2 }
     * 
     * 
     */
    public List<EhdDictionaryItemV2> getEhdDictionaryItemsV2() {
        if (ehdDictionaryItemsV2 == null) {
            ehdDictionaryItemsV2 = new ArrayList<EhdDictionaryItemV2>();
        }
        return this.ehdDictionaryItemsV2;
    }

}
