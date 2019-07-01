
package ru.mos.emias.nsiproductpublication.publicationservice.v1.types;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import ru.mos.emias.nsiproductpublication.core.v1.EhdCatalogStats;


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
 *         &lt;element name="ehdCatalogStats" type="{http://emias.mos.ru/nsiProductPublication/core/v1/}ehdCatalogStats"/&gt;
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
    "ehdCatalogStats"
})
@XmlRootElement(name = "getCatalogCountResponse")
public class GetCatalogCountResponse {

    @XmlElement(required = true)
    protected EhdCatalogStats ehdCatalogStats;

    /**
     * Gets the value of the ehdCatalogStats property.
     * 
     * @return
     *     possible object is
     *     {@link EhdCatalogStats }
     *     
     */
    public EhdCatalogStats getEhdCatalogStats() {
        return ehdCatalogStats;
    }

    /**
     * Sets the value of the ehdCatalogStats property.
     * 
     * @param value
     *     allowed object is
     *     {@link EhdCatalogStats }
     *     
     */
    public void setEhdCatalogStats(EhdCatalogStats value) {
        this.ehdCatalogStats = value;
    }

}
