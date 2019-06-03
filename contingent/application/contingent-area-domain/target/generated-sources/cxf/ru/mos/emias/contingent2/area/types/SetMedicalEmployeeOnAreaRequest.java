
package ru.mos.emias.contingent2.area.types;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import ru.mos.emias.contingent2.core.AddMedicalEmployees;
import ru.mos.emias.contingent2.core.ChangeMedicalEmployees;
import ru.mos.emias.contingent2.core.Options;


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
 *         &lt;element name="areaId" type="{http://www.w3.org/2001/XMLSchema}long"/&gt;
 *         &lt;element name="addMedicalEmployees" type="{http://emias.mos.ru/contingent2/core/v1/}AddMedicalEmployees" minOccurs="0"/&gt;
 *         &lt;element name="changeMedicalEmployees" type="{http://emias.mos.ru/contingent2/core/v1/}ChangeMedicalEmployees" minOccurs="0"/&gt;
 *         &lt;element ref="{http://emias.mos.ru/contingent2/core/v1/}options" minOccurs="0"/&gt;
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
    "areaId",
    "addMedicalEmployees",
    "changeMedicalEmployees",
    "options"
})
@XmlRootElement(name = "setMedicalEmployeeOnAreaRequest")
public class SetMedicalEmployeeOnAreaRequest
    implements Serializable
{

    private final static long serialVersionUID = 1234567890L;
    protected long areaId;
    protected AddMedicalEmployees addMedicalEmployees;
    protected ChangeMedicalEmployees changeMedicalEmployees;
    @XmlElement(namespace = "http://emias.mos.ru/contingent2/core/v1/")
    protected Options options;

    /**
     * Gets the value of the areaId property.
     * 
     */
    public long getAreaId() {
        return areaId;
    }

    /**
     * Sets the value of the areaId property.
     * 
     */
    public void setAreaId(long value) {
        this.areaId = value;
    }

    /**
     * Gets the value of the addMedicalEmployees property.
     * 
     * @return
     *     possible object is
     *     {@link AddMedicalEmployees }
     *     
     */
    public AddMedicalEmployees getAddMedicalEmployees() {
        return addMedicalEmployees;
    }

    /**
     * Sets the value of the addMedicalEmployees property.
     * 
     * @param value
     *     allowed object is
     *     {@link AddMedicalEmployees }
     *     
     */
    public void setAddMedicalEmployees(AddMedicalEmployees value) {
        this.addMedicalEmployees = value;
    }

    /**
     * Gets the value of the changeMedicalEmployees property.
     * 
     * @return
     *     possible object is
     *     {@link ChangeMedicalEmployees }
     *     
     */
    public ChangeMedicalEmployees getChangeMedicalEmployees() {
        return changeMedicalEmployees;
    }

    /**
     * Sets the value of the changeMedicalEmployees property.
     * 
     * @param value
     *     allowed object is
     *     {@link ChangeMedicalEmployees }
     *     
     */
    public void setChangeMedicalEmployees(ChangeMedicalEmployees value) {
        this.changeMedicalEmployees = value;
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
