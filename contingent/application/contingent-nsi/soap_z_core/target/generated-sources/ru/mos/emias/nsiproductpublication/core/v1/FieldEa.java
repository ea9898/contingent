
package ru.mos.emias.nsiproductpublication.core.v1;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for fieldEa complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="fieldEa"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="id" type="{http://www.w3.org/2001/XMLSchema}int"/&gt;
 *         &lt;element name="RUBRICID" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="isLayerOpen" type="{http://www.w3.org/2001/XMLSchema}int"/&gt;
 *         &lt;element name="linkData" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="icoName" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="signDate" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="isUpdateWfs" type="{http://www.w3.org/2001/XMLSchema}int"/&gt;
 *         &lt;element name="isUpdateDb" type="{http://www.w3.org/2001/XMLSchema}int"/&gt;
 *         &lt;element name="guLink" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="typeObject" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="signFio" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="isUpdate" type="{http://www.w3.org/2001/XMLSchema}int"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "fieldEa", propOrder = {
    "id",
    "rubricid",
    "isLayerOpen",
    "linkData",
    "icoName",
    "signDate",
    "isUpdateWfs",
    "isUpdateDb",
    "guLink",
    "typeObject",
    "signFio",
    "isUpdate"
})
public class FieldEa {

    @XmlElement(required = true, type = Integer.class, nillable = true)
    protected Integer id;
    @XmlElement(name = "RUBRICID", required = true)
    protected String rubricid;
    protected int isLayerOpen;
    @XmlElement(required = true)
    protected String linkData;
    @XmlElement(required = true)
    protected String icoName;
    @XmlElement(required = true)
    protected String signDate;
    protected int isUpdateWfs;
    protected int isUpdateDb;
    @XmlElement(required = true)
    protected String guLink;
    @XmlElement(required = true)
    protected String typeObject;
    @XmlElement(required = true)
    protected String signFio;
    protected int isUpdate;

    /**
     * Gets the value of the id property.
     * 
     * @return
     *     possible object is
     *     {@link Integer }
     *     
     */
    public Integer getId() {
        return id;
    }

    /**
     * Sets the value of the id property.
     * 
     * @param value
     *     allowed object is
     *     {@link Integer }
     *     
     */
    public void setId(Integer value) {
        this.id = value;
    }

    /**
     * Gets the value of the rubricid property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getRUBRICID() {
        return rubricid;
    }

    /**
     * Sets the value of the rubricid property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setRUBRICID(String value) {
        this.rubricid = value;
    }

    /**
     * Gets the value of the isLayerOpen property.
     * 
     */
    public int getIsLayerOpen() {
        return isLayerOpen;
    }

    /**
     * Sets the value of the isLayerOpen property.
     * 
     */
    public void setIsLayerOpen(int value) {
        this.isLayerOpen = value;
    }

    /**
     * Gets the value of the linkData property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getLinkData() {
        return linkData;
    }

    /**
     * Sets the value of the linkData property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setLinkData(String value) {
        this.linkData = value;
    }

    /**
     * Gets the value of the icoName property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getIcoName() {
        return icoName;
    }

    /**
     * Sets the value of the icoName property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setIcoName(String value) {
        this.icoName = value;
    }

    /**
     * Gets the value of the signDate property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getSignDate() {
        return signDate;
    }

    /**
     * Sets the value of the signDate property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setSignDate(String value) {
        this.signDate = value;
    }

    /**
     * Gets the value of the isUpdateWfs property.
     * 
     */
    public int getIsUpdateWfs() {
        return isUpdateWfs;
    }

    /**
     * Sets the value of the isUpdateWfs property.
     * 
     */
    public void setIsUpdateWfs(int value) {
        this.isUpdateWfs = value;
    }

    /**
     * Gets the value of the isUpdateDb property.
     * 
     */
    public int getIsUpdateDb() {
        return isUpdateDb;
    }

    /**
     * Sets the value of the isUpdateDb property.
     * 
     */
    public void setIsUpdateDb(int value) {
        this.isUpdateDb = value;
    }

    /**
     * Gets the value of the guLink property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getGuLink() {
        return guLink;
    }

    /**
     * Sets the value of the guLink property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setGuLink(String value) {
        this.guLink = value;
    }

    /**
     * Gets the value of the typeObject property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getTypeObject() {
        return typeObject;
    }

    /**
     * Sets the value of the typeObject property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setTypeObject(String value) {
        this.typeObject = value;
    }

    /**
     * Gets the value of the signFio property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getSignFio() {
        return signFio;
    }

    /**
     * Sets the value of the signFio property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setSignFio(String value) {
        this.signFio = value;
    }

    /**
     * Gets the value of the isUpdate property.
     * 
     */
    public int getIsUpdate() {
        return isUpdate;
    }

    /**
     * Sets the value of the isUpdate property.
     * 
     */
    public void setIsUpdate(int value) {
        this.isUpdate = value;
    }

}
