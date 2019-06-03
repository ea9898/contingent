
package ru.mos.emias.contingent2.core;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for Area complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="Area"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="id" type="{http://www.w3.org/2001/XMLSchema}long"/&gt;
 *         &lt;element name="moId" type="{http://www.w3.org/2001/XMLSchema}long"/&gt;
 *         &lt;element name="muId" type="{http://www.w3.org/2001/XMLSchema}long" minOccurs="0"/&gt;
 *         &lt;element name="number" type="{http://www.w3.org/2001/XMLSchema}int" minOccurs="0"/&gt;
 *         &lt;element name="description" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/&gt;
 *         &lt;element name="areaType" type="{http://emias.mos.ru/contingent2/core/v1/}AreaTypeShort"/&gt;
 *         &lt;element name="ageMin" type="{http://www.w3.org/2001/XMLSchema}int" minOccurs="0"/&gt;
 *         &lt;element name="ageMax" type="{http://www.w3.org/2001/XMLSchema}int" minOccurs="0"/&gt;
 *         &lt;element name="ageMinM" type="{http://www.w3.org/2001/XMLSchema}int" minOccurs="0"/&gt;
 *         &lt;element name="ageMaxM" type="{http://www.w3.org/2001/XMLSchema}int" minOccurs="0"/&gt;
 *         &lt;element name="ageMinW" type="{http://www.w3.org/2001/XMLSchema}int" minOccurs="0"/&gt;
 *         &lt;element name="ageMaxW" type="{http://www.w3.org/2001/XMLSchema}int" minOccurs="0"/&gt;
 *         &lt;element name="autoAssignForAttachment" type="{http://www.w3.org/2001/XMLSchema}boolean"/&gt;
 *         &lt;element name="attachByMedicalReason" type="{http://www.w3.org/2001/XMLSchema}boolean" minOccurs="0"/&gt;
 *         &lt;element name="primaryAreaTypeCodes" minOccurs="0"&gt;
 *           &lt;complexType&gt;
 *             &lt;complexContent&gt;
 *               &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *                 &lt;sequence&gt;
 *                   &lt;element name="areaType" type="{http://emias.mos.ru/contingent2/core/v1/}AreaTypeShort" maxOccurs="unbounded"/&gt;
 *                 &lt;/sequence&gt;
 *               &lt;/restriction&gt;
 *             &lt;/complexContent&gt;
 *           &lt;/complexType&gt;
 *         &lt;/element&gt;
 *         &lt;element name="archive" type="{http://www.w3.org/2001/XMLSchema}boolean"/&gt;
 *         &lt;element name="medicalEmployees" minOccurs="0"&gt;
 *           &lt;complexType&gt;
 *             &lt;complexContent&gt;
 *               &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *                 &lt;sequence&gt;
 *                   &lt;element name="medicalEmployee" type="{http://emias.mos.ru/contingent2/core/v1/}MedicalEmployee" maxOccurs="unbounded"/&gt;
 *                 &lt;/sequence&gt;
 *               &lt;/restriction&gt;
 *             &lt;/complexContent&gt;
 *           &lt;/complexType&gt;
 *         &lt;/element&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "Area", propOrder = {
    "id",
    "moId",
    "muId",
    "number",
    "description",
    "areaType",
    "ageMin",
    "ageMax",
    "ageMinM",
    "ageMaxM",
    "ageMinW",
    "ageMaxW",
    "autoAssignForAttachment",
    "attachByMedicalReason",
    "primaryAreaTypeCodes",
    "archive",
    "medicalEmployees"
})
public class Area
    implements Serializable
{

    private final static long serialVersionUID = 1234567890L;
    protected long id;
    protected long moId;
    protected Long muId;
    protected Integer number;
    protected String description;
    @XmlElement(required = true)
    protected AreaTypeShort areaType;
    protected Integer ageMin;
    protected Integer ageMax;
    protected Integer ageMinM;
    protected Integer ageMaxM;
    protected Integer ageMinW;
    protected Integer ageMaxW;
    protected boolean autoAssignForAttachment;
    protected Boolean attachByMedicalReason;
    protected Area.PrimaryAreaTypeCodes primaryAreaTypeCodes;
    protected boolean archive;
    protected Area.MedicalEmployees medicalEmployees;

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
     * Gets the value of the moId property.
     * 
     */
    public long getMoId() {
        return moId;
    }

    /**
     * Sets the value of the moId property.
     * 
     */
    public void setMoId(long value) {
        this.moId = value;
    }

    /**
     * Gets the value of the muId property.
     * 
     * @return
     *     possible object is
     *     {@link Long }
     *     
     */
    public Long getMuId() {
        return muId;
    }

    /**
     * Sets the value of the muId property.
     * 
     * @param value
     *     allowed object is
     *     {@link Long }
     *     
     */
    public void setMuId(Long value) {
        this.muId = value;
    }

    /**
     * Gets the value of the number property.
     * 
     * @return
     *     possible object is
     *     {@link Integer }
     *     
     */
    public Integer getNumber() {
        return number;
    }

    /**
     * Sets the value of the number property.
     * 
     * @param value
     *     allowed object is
     *     {@link Integer }
     *     
     */
    public void setNumber(Integer value) {
        this.number = value;
    }

    /**
     * Gets the value of the description property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getDescription() {
        return description;
    }

    /**
     * Sets the value of the description property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setDescription(String value) {
        this.description = value;
    }

    /**
     * Gets the value of the areaType property.
     * 
     * @return
     *     possible object is
     *     {@link AreaTypeShort }
     *     
     */
    public AreaTypeShort getAreaType() {
        return areaType;
    }

    /**
     * Sets the value of the areaType property.
     * 
     * @param value
     *     allowed object is
     *     {@link AreaTypeShort }
     *     
     */
    public void setAreaType(AreaTypeShort value) {
        this.areaType = value;
    }

    /**
     * Gets the value of the ageMin property.
     * 
     * @return
     *     possible object is
     *     {@link Integer }
     *     
     */
    public Integer getAgeMin() {
        return ageMin;
    }

    /**
     * Sets the value of the ageMin property.
     * 
     * @param value
     *     allowed object is
     *     {@link Integer }
     *     
     */
    public void setAgeMin(Integer value) {
        this.ageMin = value;
    }

    /**
     * Gets the value of the ageMax property.
     * 
     * @return
     *     possible object is
     *     {@link Integer }
     *     
     */
    public Integer getAgeMax() {
        return ageMax;
    }

    /**
     * Sets the value of the ageMax property.
     * 
     * @param value
     *     allowed object is
     *     {@link Integer }
     *     
     */
    public void setAgeMax(Integer value) {
        this.ageMax = value;
    }

    /**
     * Gets the value of the ageMinM property.
     * 
     * @return
     *     possible object is
     *     {@link Integer }
     *     
     */
    public Integer getAgeMinM() {
        return ageMinM;
    }

    /**
     * Sets the value of the ageMinM property.
     * 
     * @param value
     *     allowed object is
     *     {@link Integer }
     *     
     */
    public void setAgeMinM(Integer value) {
        this.ageMinM = value;
    }

    /**
     * Gets the value of the ageMaxM property.
     * 
     * @return
     *     possible object is
     *     {@link Integer }
     *     
     */
    public Integer getAgeMaxM() {
        return ageMaxM;
    }

    /**
     * Sets the value of the ageMaxM property.
     * 
     * @param value
     *     allowed object is
     *     {@link Integer }
     *     
     */
    public void setAgeMaxM(Integer value) {
        this.ageMaxM = value;
    }

    /**
     * Gets the value of the ageMinW property.
     * 
     * @return
     *     possible object is
     *     {@link Integer }
     *     
     */
    public Integer getAgeMinW() {
        return ageMinW;
    }

    /**
     * Sets the value of the ageMinW property.
     * 
     * @param value
     *     allowed object is
     *     {@link Integer }
     *     
     */
    public void setAgeMinW(Integer value) {
        this.ageMinW = value;
    }

    /**
     * Gets the value of the ageMaxW property.
     * 
     * @return
     *     possible object is
     *     {@link Integer }
     *     
     */
    public Integer getAgeMaxW() {
        return ageMaxW;
    }

    /**
     * Sets the value of the ageMaxW property.
     * 
     * @param value
     *     allowed object is
     *     {@link Integer }
     *     
     */
    public void setAgeMaxW(Integer value) {
        this.ageMaxW = value;
    }

    /**
     * Gets the value of the autoAssignForAttachment property.
     * 
     */
    public boolean isAutoAssignForAttachment() {
        return autoAssignForAttachment;
    }

    /**
     * Sets the value of the autoAssignForAttachment property.
     * 
     */
    public void setAutoAssignForAttachment(boolean value) {
        this.autoAssignForAttachment = value;
    }

    /**
     * Gets the value of the attachByMedicalReason property.
     * 
     * @return
     *     possible object is
     *     {@link Boolean }
     *     
     */
    public Boolean isAttachByMedicalReason() {
        return attachByMedicalReason;
    }

    /**
     * Sets the value of the attachByMedicalReason property.
     * 
     * @param value
     *     allowed object is
     *     {@link Boolean }
     *     
     */
    public void setAttachByMedicalReason(Boolean value) {
        this.attachByMedicalReason = value;
    }

    /**
     * Gets the value of the primaryAreaTypeCodes property.
     * 
     * @return
     *     possible object is
     *     {@link Area.PrimaryAreaTypeCodes }
     *     
     */
    public Area.PrimaryAreaTypeCodes getPrimaryAreaTypeCodes() {
        return primaryAreaTypeCodes;
    }

    /**
     * Sets the value of the primaryAreaTypeCodes property.
     * 
     * @param value
     *     allowed object is
     *     {@link Area.PrimaryAreaTypeCodes }
     *     
     */
    public void setPrimaryAreaTypeCodes(Area.PrimaryAreaTypeCodes value) {
        this.primaryAreaTypeCodes = value;
    }

    /**
     * Gets the value of the archive property.
     * 
     */
    public boolean isArchive() {
        return archive;
    }

    /**
     * Sets the value of the archive property.
     * 
     */
    public void setArchive(boolean value) {
        this.archive = value;
    }

    /**
     * Gets the value of the medicalEmployees property.
     * 
     * @return
     *     possible object is
     *     {@link Area.MedicalEmployees }
     *     
     */
    public Area.MedicalEmployees getMedicalEmployees() {
        return medicalEmployees;
    }

    /**
     * Sets the value of the medicalEmployees property.
     * 
     * @param value
     *     allowed object is
     *     {@link Area.MedicalEmployees }
     *     
     */
    public void setMedicalEmployees(Area.MedicalEmployees value) {
        this.medicalEmployees = value;
    }


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
     *         &lt;element name="medicalEmployee" type="{http://emias.mos.ru/contingent2/core/v1/}MedicalEmployee" maxOccurs="unbounded"/&gt;
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
        "medicalEmployees"
    })
    public static class MedicalEmployees
        implements Serializable
    {

        private final static long serialVersionUID = 1234567890L;
        @XmlElement(name = "medicalEmployee", required = true)
        protected List<MedicalEmployee> medicalEmployees;

        /**
         * Gets the value of the medicalEmployees property.
         * 
         * <p>
         * This accessor method returns a reference to the live list,
         * not a snapshot. Therefore any modification you make to the
         * returned list will be present inside the JAXB object.
         * This is why there is not a <CODE>set</CODE> method for the medicalEmployees property.
         * 
         * <p>
         * For example, to add a new item, do as follows:
         * <pre>
         *    getMedicalEmployees().add(newItem);
         * </pre>
         * 
         * 
         * <p>
         * Objects of the following type(s) are allowed in the list
         * {@link MedicalEmployee }
         * 
         * 
         */
        public List<MedicalEmployee> getMedicalEmployees() {
            if (medicalEmployees == null) {
                medicalEmployees = new ArrayList<MedicalEmployee>();
            }
            return this.medicalEmployees;
        }

    }


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
     *         &lt;element name="areaType" type="{http://emias.mos.ru/contingent2/core/v1/}AreaTypeShort" maxOccurs="unbounded"/&gt;
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
        "areaTypes"
    })
    public static class PrimaryAreaTypeCodes
        implements Serializable
    {

        private final static long serialVersionUID = 1234567890L;
        @XmlElement(name = "areaType", required = true)
        protected List<AreaTypeShort> areaTypes;

        /**
         * Gets the value of the areaTypes property.
         * 
         * <p>
         * This accessor method returns a reference to the live list,
         * not a snapshot. Therefore any modification you make to the
         * returned list will be present inside the JAXB object.
         * This is why there is not a <CODE>set</CODE> method for the areaTypes property.
         * 
         * <p>
         * For example, to add a new item, do as follows:
         * <pre>
         *    getAreaTypes().add(newItem);
         * </pre>
         * 
         * 
         * <p>
         * Objects of the following type(s) are allowed in the list
         * {@link AreaTypeShort }
         * 
         * 
         */
        public List<AreaTypeShort> getAreaTypes() {
            if (areaTypes == null) {
                areaTypes = new ArrayList<AreaTypeShort>();
            }
            return this.areaTypes;
        }

    }

}
