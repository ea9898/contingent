
package ru.mos.emias.contingent2.core;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * Сведения медработников для изменения
 * 
 * <p>Java class for ChangeMedicalEmployees complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="ChangeMedicalEmployees"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="changeMedicalEmployee" type="{http://emias.mos.ru/contingent2/core/v1/}ChangeMedicalEmployee" maxOccurs="unbounded"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "ChangeMedicalEmployees", propOrder = {
    "changeMedicalEmployees"
})
public class ChangeMedicalEmployees
    implements Serializable
{

    private final static long serialVersionUID = 1234567890L;
    @XmlElement(name = "changeMedicalEmployee", required = true)
    protected List<ChangeMedicalEmployee> changeMedicalEmployees;

    /**
     * Gets the value of the changeMedicalEmployees property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the changeMedicalEmployees property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getChangeMedicalEmployees().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link ChangeMedicalEmployee }
     * 
     * 
     */
    public List<ChangeMedicalEmployee> getChangeMedicalEmployees() {
        if (changeMedicalEmployees == null) {
            changeMedicalEmployees = new ArrayList<ChangeMedicalEmployee>();
        }
        return this.changeMedicalEmployees;
    }

}
