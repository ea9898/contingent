
package ru.mos.emias.contingent2.core;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * Сведения медработников для добавления на участок
 * 
 * <p>Java class for AddMedicalEmployees complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="AddMedicalEmployees"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="addMedicalEmployee" type="{http://emias.mos.ru/contingent2/core/v1/}AddMedicalEmployee" maxOccurs="unbounded"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "AddMedicalEmployees", propOrder = {
    "addMedicalEmployees"
})
public class AddMedicalEmployees
    implements Serializable
{

    private final static long serialVersionUID = 1234567890L;
    @XmlElement(name = "addMedicalEmployee", required = true)
    protected List<AddMedicalEmployee> addMedicalEmployees;

    /**
     * Gets the value of the addMedicalEmployees property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the addMedicalEmployees property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getAddMedicalEmployees().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link AddMedicalEmployee }
     * 
     * 
     */
    public List<AddMedicalEmployee> getAddMedicalEmployees() {
        if (addMedicalEmployees == null) {
            addMedicalEmployees = new ArrayList<AddMedicalEmployee>();
        }
        return this.addMedicalEmployees;
    }

}
