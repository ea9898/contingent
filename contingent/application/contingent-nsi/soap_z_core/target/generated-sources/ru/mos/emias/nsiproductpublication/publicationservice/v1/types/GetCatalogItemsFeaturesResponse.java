
package ru.mos.emias.nsiproductpublication.publicationservice.v1.types;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import ru.mos.emias.nsiproductpublication.core.v1.EhdCatalogItemsFeatures;


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
 *         &lt;element name="ehdCatalogItemsFeatures" type="{http://emias.mos.ru/nsiProductPublication/core/v1/}ehdCatalogItemsFeatures"/&gt;
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
    "ehdCatalogItemsFeatures"
})
@XmlRootElement(name = "getCatalogItemsFeaturesResponse")
public class GetCatalogItemsFeaturesResponse {

    @XmlElement(required = true)
    protected EhdCatalogItemsFeatures ehdCatalogItemsFeatures;

    /**
     * Gets the value of the ehdCatalogItemsFeatures property.
     * 
     * @return
     *     possible object is
     *     {@link EhdCatalogItemsFeatures }
     *     
     */
    public EhdCatalogItemsFeatures getEhdCatalogItemsFeatures() {
        return ehdCatalogItemsFeatures;
    }

    /**
     * Sets the value of the ehdCatalogItemsFeatures property.
     * 
     * @param value
     *     allowed object is
     *     {@link EhdCatalogItemsFeatures }
     *     
     */
    public void setEhdCatalogItemsFeatures(EhdCatalogItemsFeatures value) {
        this.ehdCatalogItemsFeatures = value;
    }

}
