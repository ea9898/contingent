
package ru.mos.emias.nsiproductpublication.publicationservice.v1.types;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import ru.mos.emias.nsiproductpublication.core.v1.EhdCatalogs;


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
 *         &lt;element name="ehdCatalogs" type="{http://emias.mos.ru/nsiProductPublication/core/v1/}ehdCatalogs"/&gt;
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
    "ehdCatalogs"
})
@XmlRootElement(name = "getCatalogListResponse")
public class GetCatalogListResponse {

    @XmlElement(required = true)
    protected EhdCatalogs ehdCatalogs;

    /**
     * Gets the value of the ehdCatalogs property.
     * 
     * @return
     *     possible object is
     *     {@link EhdCatalogs }
     *     
     */
    public EhdCatalogs getEhdCatalogs() {
        return ehdCatalogs;
    }

    /**
     * Sets the value of the ehdCatalogs property.
     * 
     * @param value
     *     allowed object is
     *     {@link EhdCatalogs }
     *     
     */
    public void setEhdCatalogs(EhdCatalogs value) {
        this.ehdCatalogs = value;
    }

}
