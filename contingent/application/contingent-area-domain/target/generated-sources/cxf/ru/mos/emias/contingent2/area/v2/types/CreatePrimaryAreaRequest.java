
package ru.mos.emias.contingent2.area.v2.types;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import ru.mos.emias.contingent2.core.v2.Options;


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
 *         &lt;element name="moId" type="{http://www.w3.org/2001/XMLSchema}long"/&gt;
 *         &lt;element name="muId" type="{http://www.w3.org/2001/XMLSchema}long"/&gt;
 *         &lt;element name="number" type="{http://www.w3.org/2001/XMLSchema}int" minOccurs="0"/&gt;
 *         &lt;element name="description" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/&gt;
 *         &lt;element name="areaTypeCode" type="{http://www.w3.org/2001/XMLSchema}long"/&gt;
 *         &lt;element name="ageMin" type="{http://www.w3.org/2001/XMLSchema}int" minOccurs="0"/&gt;
 *         &lt;element name="ageMax" type="{http://www.w3.org/2001/XMLSchema}int" minOccurs="0"/&gt;
 *         &lt;element name="ageMinM" type="{http://www.w3.org/2001/XMLSchema}int" minOccurs="0"/&gt;
 *         &lt;element name="ageMaxM" type="{http://www.w3.org/2001/XMLSchema}int" minOccurs="0"/&gt;
 *         &lt;element name="ageMinW" type="{http://www.w3.org/2001/XMLSchema}int" minOccurs="0"/&gt;
 *         &lt;element name="ageMaxW" type="{http://www.w3.org/2001/XMLSchema}int" minOccurs="0"/&gt;
 *         &lt;element name="autoAssignForAttachment" type="{http://www.w3.org/2001/XMLSchema}boolean"/&gt;
 *         &lt;element name="attachByMedicalReason" type="{http://www.w3.org/2001/XMLSchema}boolean" minOccurs="0"/&gt;
 *         &lt;element name="options" type="{http://emias.mos.ru/contingent2/core/v2/}Options" minOccurs="0"/&gt;
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
    "moId",
    "muId",
    "number",
    "description",
    "areaTypeCode",
    "ageMin",
    "ageMax",
    "ageMinM",
    "ageMaxM",
    "ageMinW",
    "ageMaxW",
    "autoAssignForAttachment",
    "attachByMedicalReason",
    "options"
})
@XmlRootElement(name = "createPrimaryAreaRequest")
public class CreatePrimaryAreaRequest
    implements Serializable
{

    private final static long serialVersionUID = 1234567890L;
    protected long moId;
    protected long muId;
    protected Integer number;
    protected String description;
    protected long areaTypeCode;
    protected Integer ageMin;
    protected Integer ageMax;
    protected Integer ageMinM;
    protected Integer ageMaxM;
    protected Integer ageMinW;
    protected Integer ageMaxW;
    protected boolean autoAssignForAttachment;
    protected Boolean attachByMedicalReason;
    protected Options options;

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
     */
    public long getMuId() {
        return muId;
    }

    /**
     * Sets the value of the muId property.
     * 
     */
    public void setMuId(long value) {
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
     * Gets the value of the areaTypeCode property.
     * 
     */
    public long getAreaTypeCode() {
        return areaTypeCode;
    }

    /**
     * Sets the value of the areaTypeCode property.
     * 
     */
    public void setAreaTypeCode(long value) {
        this.areaTypeCode = value;
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
