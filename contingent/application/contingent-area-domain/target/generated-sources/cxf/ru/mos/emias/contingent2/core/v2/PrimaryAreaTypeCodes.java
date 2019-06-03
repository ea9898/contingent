
package ru.mos.emias.contingent2.core.v2;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for PrimaryAreaTypeCodes complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="PrimaryAreaTypeCodes"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="primaryAreaTypeCode" type="{http://www.w3.org/2001/XMLSchema}long" maxOccurs="unbounded"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "PrimaryAreaTypeCodes", propOrder = {
    "primaryAreaTypeCodes"
})
public class PrimaryAreaTypeCodes
    implements Serializable
{

    private final static long serialVersionUID = 1234567890L;
    @XmlElement(name = "primaryAreaTypeCode", type = Long.class)
    protected List<Long> primaryAreaTypeCodes;

    /**
     * Gets the value of the primaryAreaTypeCodes property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the primaryAreaTypeCodes property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getPrimaryAreaTypeCodes().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link Long }
     * 
     * 
     */
    public List<Long> getPrimaryAreaTypeCodes() {
        if (primaryAreaTypeCodes == null) {
            primaryAreaTypeCodes = new ArrayList<Long>();
        }
        return this.primaryAreaTypeCodes;
    }

}
