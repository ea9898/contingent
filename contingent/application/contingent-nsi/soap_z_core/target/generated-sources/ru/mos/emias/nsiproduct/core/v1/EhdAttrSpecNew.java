
package ru.mos.emias.nsiproduct.core.v1;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for ehdAttrSpecNew complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="ehdAttrSpecNew"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="count" type="{http://www.w3.org/2001/XMLSchema}long"/&gt;
 *         &lt;element name="list" type="{http://emias.mos.ru/nsiProduct/core/v1/}ehdCommonAttributeNew" maxOccurs="unbounded" minOccurs="0"/&gt;
 *         &lt;element name="ehdException" type="{http://emias.mos.ru/nsiProduct/core/v1/}ehdException" maxOccurs="unbounded" minOccurs="0"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "ehdAttrSpecNew", propOrder = {
    "count",
    "list",
    "ehdException"
})
public class EhdAttrSpecNew {

    protected long count;
    protected List<EhdCommonAttributeNew> list;
    protected List<EhdException> ehdException;

    /**
     * Gets the value of the count property.
     * 
     */
    public long getCount() {
        return count;
    }

    /**
     * Sets the value of the count property.
     * 
     */
    public void setCount(long value) {
        this.count = value;
    }

    /**
     * Gets the value of the list property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the list property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getList().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link EhdCommonAttributeNew }
     * 
     * 
     */
    public List<EhdCommonAttributeNew> getList() {
        if (list == null) {
            list = new ArrayList<EhdCommonAttributeNew>();
        }
        return this.list;
    }

    /**
     * Gets the value of the ehdException property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the ehdException property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getEhdException().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link EhdException }
     * 
     * 
     */
    public List<EhdException> getEhdException() {
        if (ehdException == null) {
            ehdException = new ArrayList<EhdException>();
        }
        return this.ehdException;
    }

}
