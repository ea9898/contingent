
package ru.mos.emias.nsiproduct.nsiservice.v1.types;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import ru.mos.emias.nsiproduct.core.v1.ListResultInfo;


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
 *         &lt;element name="listResultInfo" type="{http://emias.mos.ru/nsiProduct/core/v1/}listResultInfo"/&gt;
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
    "listResultInfo"
})
@XmlRootElement(name = "setDataInResponse")
public class SetDataInResponse {

    @XmlElement(required = true)
    protected ListResultInfo listResultInfo;

    /**
     * Gets the value of the listResultInfo property.
     * 
     * @return
     *     possible object is
     *     {@link ListResultInfo }
     *     
     */
    public ListResultInfo getListResultInfo() {
        return listResultInfo;
    }

    /**
     * Sets the value of the listResultInfo property.
     * 
     * @param value
     *     allowed object is
     *     {@link ListResultInfo }
     *     
     */
    public void setListResultInfo(ListResultInfo value) {
        this.listResultInfo = value;
    }

}
