
package ru.mos.emias.nsiproductpublication.core.v1;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for ehdCatalogItemsFeatures complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="ehdCatalogItemsFeatures"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="featureList" type="{http://emias.mos.ru/nsiProductPublication/core/v1/}ehdCatalogItemsFeature" maxOccurs="unbounded" minOccurs="0"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "ehdCatalogItemsFeatures", propOrder = {
    "featureList"
})
public class EhdCatalogItemsFeatures {

    protected List<EhdCatalogItemsFeature> featureList;

    /**
     * Gets the value of the featureList property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the featureList property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getFeatureList().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link EhdCatalogItemsFeature }
     * 
     * 
     */
    public List<EhdCatalogItemsFeature> getFeatureList() {
        if (featureList == null) {
            featureList = new ArrayList<EhdCatalogItemsFeature>();
        }
        return this.featureList;
    }

}
