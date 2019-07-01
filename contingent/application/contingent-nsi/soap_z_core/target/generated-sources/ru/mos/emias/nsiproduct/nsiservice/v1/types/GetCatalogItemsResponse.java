
package ru.mos.emias.nsiproduct.nsiservice.v1.types;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import ru.mos.emias.nsiproduct.core.v1.EhdCatalogItems;
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
 *         &lt;element name="ehdCatalogItems" type="{http://emias.mos.ru/nsiProduct/core/v1/}ehdCatalogItems"/&gt;
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
    "ehdCatalogItems"
})
@XmlRootElement(name = "getCatalogItemsResponse")
public class GetCatalogItemsResponse
    extends PagingResults
{

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
