
package ru.mos.emias.nsiproductpublication.core.v1;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlList;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for ehdCommonAttribute complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="ehdCommonAttribute"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="id" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="columnId" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="name" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="enName" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="techName" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="groupName" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="typeName" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="type" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="isPk" type="{http://www.w3.org/2001/XMLSchema}boolean"/&gt;
 *         &lt;element name="isArray" type="{http://www.w3.org/2001/XMLSchema}boolean"/&gt;
 *         &lt;element name="system" type="{http://www.w3.org/2001/XMLSchema}boolean"/&gt;
 *         &lt;element name="hasTranslation" type="{http://www.w3.org/2001/XMLSchema}boolean"/&gt;
 *         &lt;element name="dictionaryId" type="{http://www.w3.org/2001/XMLSchema}int"/&gt;
 *         &lt;element name="catalogId" type="{http://www.w3.org/2001/XMLSchema}int"/&gt;
 *         &lt;element name="maxLength" type="{http://www.w3.org/2001/XMLSchema}int"/&gt;
 *         &lt;element name="systemField" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="permittedSystems" type="{http://emias.mos.ru/nsiProductPublication/core/v1/}intList"/&gt;
 *         &lt;element name="specification" type="{http://emias.mos.ru/nsiProductPublication/core/v1/}ehdCatalog"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "ehdCommonAttribute", propOrder = {
    "id",
    "columnId",
    "name",
    "enName",
    "techName",
    "groupName",
    "typeName",
    "type",
    "isPk",
    "isArray",
    "system",
    "hasTranslation",
    "dictionaryId",
    "catalogId",
    "maxLength",
    "systemField",
    "permittedSystems",
    "specification"
})
public class EhdCommonAttribute {

    @XmlElement(required = true)
    protected String id;
    @XmlElement(required = true)
    protected String columnId;
    @XmlElement(required = true)
    protected String name;
    @XmlElement(required = true)
    protected String enName;
    @XmlElement(required = true)
    protected String techName;
    @XmlElement(required = true)
    protected String groupName;
    @XmlElement(required = true)
    protected String typeName;
    @XmlElement(required = true)
    protected String type;
    @XmlElement(required = true, type = Boolean.class, nillable = true)
    protected Boolean isPk;
    @XmlElement(required = true, type = Boolean.class, nillable = true)
    protected Boolean isArray;
    @XmlElement(required = true, type = Boolean.class, nillable = true)
    protected Boolean system;
    @XmlElement(required = true, type = Boolean.class, nillable = true)
    protected Boolean hasTranslation;
    @XmlElement(required = true, type = Integer.class, nillable = true)
    protected Integer dictionaryId;
    @XmlElement(required = true, type = Integer.class, nillable = true)
    protected Integer catalogId;
    protected int maxLength;
    @XmlElement(required = true)
    protected String systemField;
    @XmlList
    @XmlElement(required = true)
    @XmlSchemaType(name = "anySimpleType")
    protected List<String> permittedSystems;
    @XmlElement(required = true)
    protected EhdCatalog specification;

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
     * Gets the value of the columnId property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getColumnId() {
        return columnId;
    }

    /**
     * Sets the value of the columnId property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setColumnId(String value) {
        this.columnId = value;
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
     * Gets the value of the enName property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getEnName() {
        return enName;
    }

    /**
     * Sets the value of the enName property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setEnName(String value) {
        this.enName = value;
    }

    /**
     * Gets the value of the techName property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getTechName() {
        return techName;
    }

    /**
     * Sets the value of the techName property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setTechName(String value) {
        this.techName = value;
    }

    /**
     * Gets the value of the groupName property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getGroupName() {
        return groupName;
    }

    /**
     * Sets the value of the groupName property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setGroupName(String value) {
        this.groupName = value;
    }

    /**
     * Gets the value of the typeName property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getTypeName() {
        return typeName;
    }

    /**
     * Sets the value of the typeName property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setTypeName(String value) {
        this.typeName = value;
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
     * Gets the value of the isPk property.
     * 
     * @return
     *     possible object is
     *     {@link Boolean }
     *     
     */
    public Boolean isIsPk() {
        return isPk;
    }

    /**
     * Sets the value of the isPk property.
     * 
     * @param value
     *     allowed object is
     *     {@link Boolean }
     *     
     */
    public void setIsPk(Boolean value) {
        this.isPk = value;
    }

    /**
     * Gets the value of the isArray property.
     * 
     * @return
     *     possible object is
     *     {@link Boolean }
     *     
     */
    public Boolean isIsArray() {
        return isArray;
    }

    /**
     * Sets the value of the isArray property.
     * 
     * @param value
     *     allowed object is
     *     {@link Boolean }
     *     
     */
    public void setIsArray(Boolean value) {
        this.isArray = value;
    }

    /**
     * Gets the value of the system property.
     * 
     * @return
     *     possible object is
     *     {@link Boolean }
     *     
     */
    public Boolean isSystem() {
        return system;
    }

    /**
     * Sets the value of the system property.
     * 
     * @param value
     *     allowed object is
     *     {@link Boolean }
     *     
     */
    public void setSystem(Boolean value) {
        this.system = value;
    }

    /**
     * Gets the value of the hasTranslation property.
     * 
     * @return
     *     possible object is
     *     {@link Boolean }
     *     
     */
    public Boolean isHasTranslation() {
        return hasTranslation;
    }

    /**
     * Sets the value of the hasTranslation property.
     * 
     * @param value
     *     allowed object is
     *     {@link Boolean }
     *     
     */
    public void setHasTranslation(Boolean value) {
        this.hasTranslation = value;
    }

    /**
     * Gets the value of the dictionaryId property.
     * 
     * @return
     *     possible object is
     *     {@link Integer }
     *     
     */
    public Integer getDictionaryId() {
        return dictionaryId;
    }

    /**
     * Sets the value of the dictionaryId property.
     * 
     * @param value
     *     allowed object is
     *     {@link Integer }
     *     
     */
    public void setDictionaryId(Integer value) {
        this.dictionaryId = value;
    }

    /**
     * Gets the value of the catalogId property.
     * 
     * @return
     *     possible object is
     *     {@link Integer }
     *     
     */
    public Integer getCatalogId() {
        return catalogId;
    }

    /**
     * Sets the value of the catalogId property.
     * 
     * @param value
     *     allowed object is
     *     {@link Integer }
     *     
     */
    public void setCatalogId(Integer value) {
        this.catalogId = value;
    }

    /**
     * Gets the value of the maxLength property.
     * 
     */
    public int getMaxLength() {
        return maxLength;
    }

    /**
     * Sets the value of the maxLength property.
     * 
     */
    public void setMaxLength(int value) {
        this.maxLength = value;
    }

    /**
     * Gets the value of the systemField property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getSystemField() {
        return systemField;
    }

    /**
     * Sets the value of the systemField property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setSystemField(String value) {
        this.systemField = value;
    }

    /**
     * Gets the value of the permittedSystems property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the permittedSystems property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getPermittedSystems().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link String }
     * 
     * 
     */
    public List<String> getPermittedSystems() {
        if (permittedSystems == null) {
            permittedSystems = new ArrayList<String>();
        }
        return this.permittedSystems;
    }

    /**
     * Gets the value of the specification property.
     * 
     * @return
     *     possible object is
     *     {@link EhdCatalog }
     *     
     */
    public EhdCatalog getSpecification() {
        return specification;
    }

    /**
     * Sets the value of the specification property.
     * 
     * @param value
     *     allowed object is
     *     {@link EhdCatalog }
     *     
     */
    public void setSpecification(EhdCatalog value) {
        this.specification = value;
    }

}
