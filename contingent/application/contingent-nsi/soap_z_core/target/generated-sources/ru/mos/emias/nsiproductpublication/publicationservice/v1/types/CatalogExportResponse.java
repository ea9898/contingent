
package ru.mos.emias.nsiproductpublication.publicationservice.v1.types;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import ru.mos.emias.nsiproductpublication.core.v1.CatalogExport;


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
 *         &lt;element name="catalogExports" type="{http://emias.mos.ru/nsiProductPublication/core/v1/}catalogExport"/&gt;
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
    "catalogExports"
})
@XmlRootElement(name = "catalogExportResponse")
public class CatalogExportResponse {

    @XmlElement(required = true)
    protected CatalogExport catalogExports;

    /**
     * Gets the value of the catalogExports property.
     * 
     * @return
     *     possible object is
     *     {@link CatalogExport }
     *     
     */
    public CatalogExport getCatalogExports() {
        return catalogExports;
    }

    /**
     * Sets the value of the catalogExports property.
     * 
     * @param value
     *     allowed object is
     *     {@link CatalogExport }
     *     
     */
    public void setCatalogExports(CatalogExport value) {
        this.catalogExports = value;
    }

}
