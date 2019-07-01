
package ru.mos.op.receive_changes;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;


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
 *         &lt;element name="in" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="intype" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
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
    "in",
    "intype"
})
@XmlRootElement(name = "ChangeElement")
public class ChangeElement {

    @XmlElement(required = true)
    protected String in;
    @XmlElement(required = true, nillable = true)
    protected String intype;

    /**
     * Gets the value of the in property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getIn() {
        return in;
    }

    /**
     * Sets the value of the in property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setIn(String value) {
        this.in = value;
    }

    /**
     * Gets the value of the intype property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getIntype() {
        return intype;
    }

    /**
     * Sets the value of the intype property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setIntype(String value) {
        this.intype = value;
    }

}
