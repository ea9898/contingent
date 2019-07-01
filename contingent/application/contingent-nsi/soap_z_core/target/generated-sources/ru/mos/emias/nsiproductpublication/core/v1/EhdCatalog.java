
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
 * <p>Java class for ehdCatalog complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="ehdCatalog"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="id" type="{http://www.w3.org/2001/XMLSchema}long"/&gt;
 *         &lt;element name="name" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="fullName" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="technicalName" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="shortName" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="accountingObject" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="hasEnVersion" type="{http://www.w3.org/2001/XMLSchema}boolean"/&gt;
 *         &lt;element name="keywords" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="vid" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="type" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="kind" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="period" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="hasGeo" type="{http://www.w3.org/2001/XMLSchema}boolean"/&gt;
 *         &lt;element name="layerId" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="categories" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="OIV" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="oivList" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="description" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="CNT" type="{http://www.w3.org/2001/XMLSchema}long"/&gt;
 *         &lt;element name="permittedSystems" type="{http://emias.mos.ru/nsiProductPublication/core/v1/}intList"/&gt;
 *         &lt;element name="fields" type="{http://emias.mos.ru/nsiProductPublication/core/v1/}ehdCommonAttribute" maxOccurs="unbounded" minOccurs="0"/&gt;
 *         &lt;element name="systemFields" type="{http://emias.mos.ru/nsiProductPublication/core/v1/}systemField" maxOccurs="unbounded" minOccurs="0"/&gt;
 *         &lt;element name="fieldsEa" type="{http://emias.mos.ru/nsiProductPublication/core/v1/}fieldEa"/&gt;
 *         &lt;element name="responsiblePerson" type="{http://emias.mos.ru/nsiProductPublication/core/v1/}responsiblPerson"/&gt;
 *         &lt;element name="nameVariants" type="{http://emias.mos.ru/nsiProductPublication/core/v1/}nameVariant" maxOccurs="unbounded" minOccurs="0"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "ehdCatalog", propOrder = {
    "id",
    "name",
    "fullName",
    "technicalName",
    "shortName",
    "accountingObject",
    "hasEnVersion",
    "keywords",
    "vid",
    "type",
    "kind",
    "period",
    "hasGeo",
    "layerId",
    "categories",
    "oiv",
    "oivList",
    "description",
    "cnt",
    "permittedSystems",
    "fields",
    "systemFields",
    "fieldsEa",
    "responsiblePerson",
    "nameVariants"
})
public class EhdCatalog {

    @XmlElement(required = true, type = Long.class, nillable = true)
    protected Long id;
    @XmlElement(required = true)
    protected String name;
    @XmlElement(required = true)
    protected String fullName;
    @XmlElement(required = true)
    protected String technicalName;
    @XmlElement(required = true)
    protected String shortName;
    @XmlElement(required = true)
    protected String accountingObject;
    protected boolean hasEnVersion;
    @XmlElement(required = true)
    protected String keywords;
    @XmlElement(required = true)
    protected String vid;
    @XmlElement(required = true)
    protected String type;
    @XmlElement(required = true)
    protected String kind;
    @XmlElement(required = true)
    protected String period;
    @XmlElement(required = true, type = Boolean.class, nillable = true)
    protected Boolean hasGeo;
    @XmlElement(required = true)
    protected String layerId;
    @XmlElement(required = true)
    protected String categories;
    @XmlElement(name = "OIV", required = true)
    protected String oiv;
    @XmlElement(required = true)
    protected String oivList;
    @XmlElement(required = true)
    protected String description;
    @XmlElement(name = "CNT")
    protected long cnt;
    @XmlList
    @XmlElement(required = true)
    @XmlSchemaType(name = "anySimpleType")
    protected List<String> permittedSystems;
    protected List<EhdCommonAttribute> fields;
    protected List<SystemField> systemFields;
    @XmlElement(required = true)
    protected FieldEa fieldsEa;
    @XmlElement(required = true)
    protected ResponsiblPerson responsiblePerson;
    protected List<NameVariant> nameVariants;

    /**
     * Gets the value of the id property.
     * 
     * @return
     *     possible object is
     *     {@link Long }
     *     
     */
    public Long getId() {
        return id;
    }

    /**
     * Sets the value of the id property.
     * 
     * @param value
     *     allowed object is
     *     {@link Long }
     *     
     */
    public void setId(Long value) {
        this.id = value;
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
     * Gets the value of the fullName property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getFullName() {
        return fullName;
    }

    /**
     * Sets the value of the fullName property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setFullName(String value) {
        this.fullName = value;
    }

    /**
     * Gets the value of the technicalName property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getTechnicalName() {
        return technicalName;
    }

    /**
     * Sets the value of the technicalName property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setTechnicalName(String value) {
        this.technicalName = value;
    }

    /**
     * Gets the value of the shortName property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getShortName() {
        return shortName;
    }

    /**
     * Sets the value of the shortName property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setShortName(String value) {
        this.shortName = value;
    }

    /**
     * Gets the value of the accountingObject property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getAccountingObject() {
        return accountingObject;
    }

    /**
     * Sets the value of the accountingObject property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setAccountingObject(String value) {
        this.accountingObject = value;
    }

    /**
     * Gets the value of the hasEnVersion property.
     * 
     */
    public boolean isHasEnVersion() {
        return hasEnVersion;
    }

    /**
     * Sets the value of the hasEnVersion property.
     * 
     */
    public void setHasEnVersion(boolean value) {
        this.hasEnVersion = value;
    }

    /**
     * Gets the value of the keywords property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getKeywords() {
        return keywords;
    }

    /**
     * Sets the value of the keywords property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setKeywords(String value) {
        this.keywords = value;
    }

    /**
     * Gets the value of the vid property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getVid() {
        return vid;
    }

    /**
     * Sets the value of the vid property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setVid(String value) {
        this.vid = value;
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
     * Gets the value of the kind property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getKind() {
        return kind;
    }

    /**
     * Sets the value of the kind property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setKind(String value) {
        this.kind = value;
    }

    /**
     * Gets the value of the period property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getPeriod() {
        return period;
    }

    /**
     * Sets the value of the period property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setPeriod(String value) {
        this.period = value;
    }

    /**
     * Gets the value of the hasGeo property.
     * 
     * @return
     *     possible object is
     *     {@link Boolean }
     *     
     */
    public Boolean isHasGeo() {
        return hasGeo;
    }

    /**
     * Sets the value of the hasGeo property.
     * 
     * @param value
     *     allowed object is
     *     {@link Boolean }
     *     
     */
    public void setHasGeo(Boolean value) {
        this.hasGeo = value;
    }

    /**
     * Gets the value of the layerId property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getLayerId() {
        return layerId;
    }

    /**
     * Sets the value of the layerId property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setLayerId(String value) {
        this.layerId = value;
    }

    /**
     * Gets the value of the categories property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getCategories() {
        return categories;
    }

    /**
     * Sets the value of the categories property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCategories(String value) {
        this.categories = value;
    }

    /**
     * Gets the value of the oiv property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getOIV() {
        return oiv;
    }

    /**
     * Sets the value of the oiv property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setOIV(String value) {
        this.oiv = value;
    }

    /**
     * Gets the value of the oivList property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getOivList() {
        return oivList;
    }

    /**
     * Sets the value of the oivList property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setOivList(String value) {
        this.oivList = value;
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
     * Gets the value of the cnt property.
     * 
     */
    public long getCNT() {
        return cnt;
    }

    /**
     * Sets the value of the cnt property.
     * 
     */
    public void setCNT(long value) {
        this.cnt = value;
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
     * Gets the value of the fields property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the fields property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getFields().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link EhdCommonAttribute }
     * 
     * 
     */
    public List<EhdCommonAttribute> getFields() {
        if (fields == null) {
            fields = new ArrayList<EhdCommonAttribute>();
        }
        return this.fields;
    }

    /**
     * Gets the value of the systemFields property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the systemFields property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getSystemFields().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link SystemField }
     * 
     * 
     */
    public List<SystemField> getSystemFields() {
        if (systemFields == null) {
            systemFields = new ArrayList<SystemField>();
        }
        return this.systemFields;
    }

    /**
     * Gets the value of the fieldsEa property.
     * 
     * @return
     *     possible object is
     *     {@link FieldEa }
     *     
     */
    public FieldEa getFieldsEa() {
        return fieldsEa;
    }

    /**
     * Sets the value of the fieldsEa property.
     * 
     * @param value
     *     allowed object is
     *     {@link FieldEa }
     *     
     */
    public void setFieldsEa(FieldEa value) {
        this.fieldsEa = value;
    }

    /**
     * Gets the value of the responsiblePerson property.
     * 
     * @return
     *     possible object is
     *     {@link ResponsiblPerson }
     *     
     */
    public ResponsiblPerson getResponsiblePerson() {
        return responsiblePerson;
    }

    /**
     * Sets the value of the responsiblePerson property.
     * 
     * @param value
     *     allowed object is
     *     {@link ResponsiblPerson }
     *     
     */
    public void setResponsiblePerson(ResponsiblPerson value) {
        this.responsiblePerson = value;
    }

    /**
     * Gets the value of the nameVariants property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the nameVariants property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getNameVariants().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link NameVariant }
     * 
     * 
     */
    public List<NameVariant> getNameVariants() {
        if (nameVariants == null) {
            nameVariants = new ArrayList<NameVariant>();
        }
        return this.nameVariants;
    }

}
