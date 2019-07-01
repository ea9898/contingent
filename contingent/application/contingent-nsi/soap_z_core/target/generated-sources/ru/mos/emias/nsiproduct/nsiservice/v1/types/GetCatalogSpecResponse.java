
package ru.mos.emias.nsiproduct.nsiservice.v1.types;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import ru.mos.emias.nsiproduct.core.v1.EhdAttrSpec;


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
 *         &lt;element name="ehdAttrSpec" type="{http://emias.mos.ru/nsiProduct/core/v1/}ehdAttrSpec"/&gt;
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
    "ehdAttrSpec"
})
@XmlRootElement(name = "getCatalogSpecResponse")
public class GetCatalogSpecResponse {

    @XmlElement(required = true)
    protected EhdAttrSpec ehdAttrSpec;

    /**
     * Gets the value of the ehdAttrSpec property.
     * 
     * @return
     *     possible object is
     *     {@link EhdAttrSpec }
     *     
     */
    public EhdAttrSpec getEhdAttrSpec() {
        return ehdAttrSpec;
    }

    /**
     * Sets the value of the ehdAttrSpec property.
     * 
     * @param value
     *     allowed object is
     *     {@link EhdAttrSpec }
     *     
     */
    public void setEhdAttrSpec(EhdAttrSpec value) {
        this.ehdAttrSpec = value;
    }

}
