
package ru.mos.emias.formproduct.formservice.v1.types;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import ru.mos.emias.formproduct.core.v1.Options;


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
 *         &lt;element name="inSearchFormId" type="{http://www.w3.org/2001/XMLSchema}int"/&gt;
 *         &lt;element ref="{http://emias.mos.ru/formProduct/core/v1/}options" minOccurs="0"/&gt;
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
    "inSearchFormId",
    "options"
})
@XmlRootElement(name = "getFieldsByFormIdRequest")
public class GetFieldsByFormIdRequest {

    protected int inSearchFormId;
    @XmlElement(namespace = "http://emias.mos.ru/formProduct/core/v1/")
    protected Options options;

    /**
     * Gets the value of the inSearchFormId property.
     * 
     */
    public int getInSearchFormId() {
        return inSearchFormId;
    }

    /**
     * Sets the value of the inSearchFormId property.
     * 
     */
    public void setInSearchFormId(int value) {
        this.inSearchFormId = value;
    }

    /**
     * Gets the value of the options property.
     * 
     * @return
     *     possible object is
     *     {@link Options }
     *     
     */
    public Options getOptions() {
        return options;
    }

    /**
     * Sets the value of the options property.
     * 
     * @param value
     *     allowed object is
     *     {@link Options }
     *     
     */
    public void setOptions(Options value) {
        this.options = value;
    }

}
