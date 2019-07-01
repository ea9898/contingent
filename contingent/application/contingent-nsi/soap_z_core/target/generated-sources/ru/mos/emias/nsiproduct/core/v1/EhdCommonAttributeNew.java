
package ru.mos.emias.nsiproduct.core.v1;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for ehdCommonAttributeNew complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="ehdCommonAttributeNew"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="id" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="typeId" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="name" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="colname" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="type" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="isPrimaryKey" type="{http://www.w3.org/2001/XMLSchema}boolean"/&gt;
 *         &lt;element name="isEdit" type="{http://www.w3.org/2001/XMLSchema}boolean"/&gt;
 *         &lt;element name="isReq" type="{http://www.w3.org/2001/XMLSchema}boolean"/&gt;
 *         &lt;element name="fieldMask" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="tehName" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="maxLength" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="maxLengthDecimal" type="{http://www.w3.org/2001/XMLSchema}int"/&gt;
 *         &lt;element name="dictId" type="{http://www.w3.org/2001/XMLSchema}int"/&gt;
 *         &lt;element name="refCatalog" type="{http://www.w3.org/2001/XMLSchema}int"/&gt;
 *         &lt;element name="isDeleted" type="{http://www.w3.org/2001/XMLSchema}int"/&gt;
 *         &lt;element name="isDeletedTmp" type="{http://www.w3.org/2001/XMLSchema}int"/&gt;
 *         &lt;element name="isMulti" type="{http://www.w3.org/2001/XMLSchema}boolean"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "ehdCommonAttributeNew", propOrder = {
    "id",
    "typeId",
    "name",
    "colname",
    "type",
    "isPrimaryKey",
    "isEdit",
    "isReq",
    "fieldMask",
    "tehName",
    "maxLength",
    "maxLengthDecimal",
    "dictId",
    "refCatalog",
    "isDeleted",
    "isDeletedTmp",
    "isMulti"
})
public class EhdCommonAttributeNew {

    @XmlElement(required = true)
    protected String id;
    @XmlElement(required = true)
    protected String typeId;
    @XmlElement(required = true)
    protected String name;
    @XmlElement(required = true)
    protected String colname;
    @XmlElement(required = true)
    protected String type;
    @XmlElement(required = true, type = Boolean.class, nillable = true)
    protected Boolean isPrimaryKey;
    @XmlElement(required = true, type = Boolean.class, nillable = true)
    protected Boolean isEdit;
    @XmlElement(required = true, type = Boolean.class, nillable = true)
    protected Boolean isReq;
    @XmlElement(required = true)
    protected String fieldMask;
    @XmlElement(required = true)
    protected String tehName;
    @XmlElement(required = true)
    protected String maxLength;
    @XmlElement(required = true, type = Integer.class, nillable = true)
    protected Integer maxLengthDecimal;
    @XmlElement(required = true, type = Integer.class, nillable = true)
    protected Integer dictId;
    @XmlElement(required = true, type = Integer.class, nillable = true)
    protected Integer refCatalog;
    @XmlElement(required = true, type = Integer.class, nillable = true)
    protected Integer isDeleted;
    @XmlElement(required = true, type = Integer.class, nillable = true)
    protected Integer isDeletedTmp;
    protected boolean isMulti;

    /**
     * Gets the value of the id property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getId() {
        return id;
    }

    /**
     * Sets the value of the id property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setId(String value) {
        this.id = value;
    }

    /**
     * Gets the value of the typeId property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getTypeId() {
        return typeId;
    }

    /**
     * Sets the value of the typeId property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setTypeId(String value) {
        this.typeId = value;
    }

    /**
     * Gets the value of the name property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getName() {
        return name;
    }

    /**
     * Sets the value of the name property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setName(String value) {
        this.name = value;
    }

    /**
     * Gets the value of the colname property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getColname() {
        return colname;
    }

    /**
     * Sets the value of the colname property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setColname(String value) {
        this.colname = value;
    }

    /**
     * Gets the value of the type property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getType() {
        return type;
    }

    /**
     * Sets the value of the type property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setType(String value) {
        this.type = value;
    }

    /**
     * Gets the value of the isPrimaryKey property.
     * 
     * @return
     *     possible object is
     *     {@link Boolean }
     *     
     */
    public Boolean isIsPrimaryKey() {
        return isPrimaryKey;
    }

    /**
     * Sets the value of the isPrimaryKey property.
     * 
     * @param value
     *     allowed object is
     *     {@link Boolean }
     *     
     */
    public void setIsPrimaryKey(Boolean value) {
        this.isPrimaryKey = value;
    }

    /**
     * Gets the value of the isEdit property.
     * 
     * @return
     *     possible object is
     *     {@link Boolean }
     *     
     */
    public Boolean isIsEdit() {
        return isEdit;
    }

    /**
     * Sets the value of the isEdit property.
     * 
     * @param value
     *     allowed object is
     *     {@link Boolean }
     *     
     */
    public void setIsEdit(Boolean value) {
        this.isEdit = value;
    }

    /**
     * Gets the value of the isReq property.
     * 
     * @return
     *     possible object is
     *     {@link Boolean }
     *     
     */
    public Boolean isIsReq() {
        return isReq;
    }

    /**
     * Sets the value of the isReq property.
     * 
     * @param value
     *     allowed object is
     *     {@link Boolean }
     *     
     */
    public void setIsReq(Boolean value) {
        this.isReq = value;
    }

    /**
     * Gets the value of the fieldMask property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getFieldMask() {
        return fieldMask;
    }

    /**
     * Sets the value of the fieldMask property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setFieldMask(String value) {
        this.fieldMask = value;
    }

    /**
     * Gets the value of the tehName property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getTehName() {
        return tehName;
    }

    /**
     * Sets the value of the tehName property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setTehName(String value) {
        this.tehName = value;
    }

    /**
     * Gets the value of the maxLength property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getMaxLength() {
        return maxLength;
    }

    /**
     * Sets the value of the maxLength property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setMaxLength(String value) {
        this.maxLength = value;
    }

    /**
     * Gets the value of the maxLengthDecimal property.
     * 
     * @return
     *     possible object is
     *     {@link Integer }
     *     
     */
    public Integer getMaxLengthDecimal() {
        return maxLengthDecimal;
    }

    /**
     * Sets the value of the maxLengthDecimal property.
     * 
     * @param value
     *     allowed object is
     *     {@link Integer }
     *     
     */
    public void setMaxLengthDecimal(Integer value) {
        this.maxLengthDecimal = value;
    }

    /**
     * Gets the value of the dictId property.
     * 
     * @return
     *     possible object is
     *     {@link Integer }
     *     
     */
    public Integer getDictId() {
        return dictId;
    }

    /**
     * Sets the value of the dictId property.
     * 
     * @param value
     *     allowed object is
     *     {@link Integer }
     *     
     */
    public void setDictId(Integer value) {
        this.dictId = value;
    }

    /**
     * Gets the value of the refCatalog property.
     * 
     * @return
     *     possible object is
     *     {@link Integer }
     *     
     */
    public Integer getRefCatalog() {
        return refCatalog;
    }

    /**
     * Sets the value of the refCatalog property.
     * 
     * @param value
     *     allowed object is
     *     {@link Integer }
     *     
     */
    public void setRefCatalog(Integer value) {
        this.refCatalog = value;
    }

    /**
     * Gets the value of the isDeleted property.
     * 
     * @return
     *     possible object is
     *     {@link Integer }
     *     
     */
    public Integer getIsDeleted() {
        return isDeleted;
    }

    /**
     * Sets the value of the isDeleted property.
     * 
     * @param value
     *     allowed object is
     *     {@link Integer }
     *     
     */
    public void setIsDeleted(Integer value) {
        this.isDeleted = value;
    }

    /**
     * Gets the value of the isDeletedTmp property.
     * 
     * @return
     *     possible object is
     *     {@link Integer }
     *     
     */
    public Integer getIsDeletedTmp() {
        return isDeletedTmp;
    }

    /**
     * Sets the value of the isDeletedTmp property.
     * 
     * @param value
     *     allowed object is
     *     {@link Integer }
     *     
     */
    public void setIsDeletedTmp(Integer value) {
        this.isDeletedTmp = value;
    }

    /**
     * Gets the value of the isMulti property.
     * 
     */
    public boolean isIsMulti() {
        return isMulti;
    }

    /**
     * Sets the value of the isMulti property.
     * 
     */
    public void setIsMulti(boolean value) {
        this.isMulti = value;
    }

}
