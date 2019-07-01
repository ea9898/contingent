
package ru.mos.emias.nsiproductpublication.publicationservice.v1.types;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import ru.mos.emias.nsiproductpublication.core.v1.EhdCatalogItems;


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
 *         &lt;element name="ehdCatalogItems" type="{http://emias.mos.ru/nsiProductPublication/core/v1/}ehdCatalogItems"/&gt;
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
    "ehdCatalogItems"
})
@XmlRootElement(name = "getCatalogItemsResponse")
public class GetCatalogItemsResponse {

    @XmlElement(required = true)
    protected EhdCatalogItems ehdCatalogItems;

    /**
     * Gets the value of the ehdCatalogItems property.
     * 
     * @return
     *     possible object is
     *     {@link EhdCatalogItems }
     *     
     */
    public EhdCatalogItems getEhdCatalogItems() {
        return ehdCatalogItems;
    }

    /**
     * Sets the value of the ehdCatalogItems property.
     * 
     * @param value
     *     allowed object is
     *     {@link EhdCatalogItems }
     *     
     */
    public void setEhdCatalogItems(EhdCatalogItems value) {
        this.ehdCatalogItems = value;
    }

}
