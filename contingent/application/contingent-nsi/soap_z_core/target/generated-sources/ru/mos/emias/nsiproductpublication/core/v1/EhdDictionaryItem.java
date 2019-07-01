
package ru.mos.emias.nsiproductpublication.core.v1;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for ehdDictionaryItem complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="ehdDictionaryItem"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="key" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="prnt_key" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="value" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="value_en" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "ehdDictionaryItem", propOrder = {
    "key",
    "prntKey",
    "value",
    "valueEn"
})
public class EhdDictionaryItem {

    @XmlElement(required = true)
    protected String key;
    @XmlElement(name = "prnt_key", required = true)
    protected String prntKey;
    @XmlElement(required = true)
    protected String value;
    @XmlElement(name = "value_en", required = true)
    protected String valueEn;

    /**
     * Gets the value of the key property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getKey() {
        return key;
    }

    /**
     * Sets the value of the key property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setKey(String value) {
        this.key = value;
    }

    /**
     * Gets the value of the prntKey property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getPrntKey() {
        return prntKey;
    }

    /**
     * Sets the value of the prntKey property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setPrntKey(String value) {
        this.prntKey = value;
    }

    /**
     * Gets the value of the value property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getValue() {
        return value;
    }

    /**
     * Sets the value of the value property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setValue(String value) {
        this.value = value;
    }

    /**
     * Gets the value of the valueEn property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getValueEn() {
        return valueEn;
    }

    /**
     * Sets the value of the valueEn property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setValueEn(String value) {
        this.valueEn = value;
    }

}
