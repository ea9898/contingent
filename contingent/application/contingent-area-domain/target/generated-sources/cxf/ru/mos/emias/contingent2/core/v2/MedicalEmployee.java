
package ru.mos.emias.contingent2.core.v2;

import java.io.Serializable;
import java.time.LocalDate;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import org.w3._2001.xmlschema.Adapter3;


/**
 * <p>Java class for MedicalEmployee complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="MedicalEmployee"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="id" type="{http://www.w3.org/2001/XMLSchema}long"/&gt;
 *         &lt;element name="medicalEmployeeJobInfoId" type="{http://www.w3.org/2001/XMLSchema}long"/&gt;
 *         &lt;element name="snils" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="positionId" type="{http://www.w3.org/2001/XMLSchema}long"/&gt;
 *         &lt;element name="isReplacement" type="{http://www.w3.org/2001/XMLSchema}boolean"/&gt;
 *         &lt;element name="startDate" type="{http://www.w3.org/2001/XMLSchema}date" minOccurs="0"/&gt;
 *         &lt;element name="endDate" type="{http://www.w3.org/2001/XMLSchema}date" minOccurs="0"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "MedicalEmployee", propOrder = {
    "id",
    "medicalEmployeeJobInfoId",
    "snils",
    "positionId",
    "isReplacement",
    "startDate",
    "endDate"
})
public class MedicalEmployee
    implements Serializable
{

    private final static long serialVersionUID = 1234567890L;
    protected long id;
    protected long medicalEmployeeJobInfoId;
    @XmlElement(required = true)
    protected String snils;
    protected long positionId;
    protected boolean isReplacement;
    @XmlElement(type = String.class)
    @XmlJavaTypeAdapter(Adapter3 .class)
    @XmlSchemaType(name = "date")
    protected LocalDate startDate;
    @XmlElement(type = String.class)
    @XmlJavaTypeAdapter(Adapter3 .class)
    @XmlSchemaType(name = "date")
    protected LocalDate endDate;

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
     * Gets the value of the medicalEmployeeJobInfoId property.
     * 
     */
    public long getMedicalEmployeeJobInfoId() {
        return medicalEmployeeJobInfoId;
    }

    /**
     * Sets the value of the medicalEmployeeJobInfoId property.
     * 
     */
    public void setMedicalEmployeeJobInfoId(long value) {
        this.medicalEmployeeJobInfoId = value;
    }

    /**
     * Gets the value of the snils property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getSnils() {
        return snils;
    }

    /**
     * Sets the value of the snils property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setSnils(String value) {
        this.snils = value;
    }

    /**
     * Gets the value of the positionId property.
     * 
     */
    public long getPositionId() {
        return positionId;
    }

    /**
     * Sets the value of the positionId property.
     * 
     */
    public void setPositionId(long value) {
        this.positionId = value;
    }

    /**
     * Gets the value of the isReplacement property.
     * 
     */
    public boolean isIsReplacement() {
        return isReplacement;
    }

    /**
     * Sets the value of the isReplacement property.
     * 
     */
    public void setIsReplacement(boolean value) {
        this.isReplacement = value;
    }

    /**
     * Gets the value of the startDate property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public LocalDate getStartDate() {
        return startDate;
    }

    /**
     * Sets the value of the startDate property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setStartDate(LocalDate value) {
        this.startDate = value;
    }

    /**
     * Gets the value of the endDate property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public LocalDate getEndDate() {
        return endDate;
    }

    /**
     * Sets the value of the endDate property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setEndDate(LocalDate value) {
        this.endDate = value;
    }

}
