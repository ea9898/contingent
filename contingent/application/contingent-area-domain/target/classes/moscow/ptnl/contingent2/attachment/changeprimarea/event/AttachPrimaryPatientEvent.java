//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, v2.3.0 
// See <a href="https://javaee.github.io/jaxb-v2/">https://javaee.github.io/jaxb-v2/</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2019.06.03 at 03:27:15 PM MSK 
//


package moscow.ptnl.contingent2.attachment.changeprimarea.event;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlType;
import javax.xml.datatype.XMLGregorianCalendar;


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
 *         &lt;element name="id" type="{http://ptnl.moscow/contingent/event/type/attachment/}ID"/&gt;
 *         &lt;element name="operationDate" type="{http://www.w3.org/2001/XMLSchema}dateTime"/&gt;
 *         &lt;element name="patientEmiasId" type="{http://ptnl.moscow/contingent/event/type/attachment/}ID"/&gt;
 *         &lt;element name="policyType" type="{http://ptnl.moscow/contingent/event/type/attachment/}ID"/&gt;
 *         &lt;element name="primaryAreaId" type="{http://ptnl.moscow/contingent/event/type/attachment/}ID"/&gt;
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
    "id",
    "operationDate",
    "patientEmiasId",
    "policyType",
    "primaryAreaId"
})
@XmlRootElement(name = "attachPrimaryPatientEvent")
public class AttachPrimaryPatientEvent {

    protected long id;
    @XmlElement(required = true)
    @XmlSchemaType(name = "dateTime")
    protected XMLGregorianCalendar operationDate;
    protected long patientEmiasId;
    protected long policyType;
    protected long primaryAreaId;

    /**
     * Gets the value of the id property.
     * 
     */
    public long getId() {
        return id;
    }

    /**
     * Sets the value of the id property.
     * 
     */
    public void setId(long value) {
        this.id = value;
    }

    /**
     * Gets the value of the operationDate property.
     * 
     * @return
     *     possible object is
     *     {@link XMLGregorianCalendar }
     *     
     */
    public XMLGregorianCalendar getOperationDate() {
        return operationDate;
    }

    /**
     * Sets the value of the operationDate property.
     * 
     * @param value
     *     allowed object is
     *     {@link XMLGregorianCalendar }
     *     
     */
    public void setOperationDate(XMLGregorianCalendar value) {
        this.operationDate = value;
    }

    /**
     * Gets the value of the patientEmiasId property.
     * 
     */
    public long getPatientEmiasId() {
        return patientEmiasId;
    }

    /**
     * Sets the value of the patientEmiasId property.
     * 
     */
    public void setPatientEmiasId(long value) {
        this.patientEmiasId = value;
    }

    /**
     * Gets the value of the policyType property.
     * 
     */
    public long getPolicyType() {
        return policyType;
    }

    /**
     * Sets the value of the policyType property.
     * 
     */
    public void setPolicyType(long value) {
        this.policyType = value;
    }

    /**
     * Gets the value of the primaryAreaId property.
     * 
     */
    public long getPrimaryAreaId() {
        return primaryAreaId;
    }

    /**
     * Sets the value of the primaryAreaId property.
     * 
     */
    public void setPrimaryAreaId(long value) {
        this.primaryAreaId = value;
    }

}
