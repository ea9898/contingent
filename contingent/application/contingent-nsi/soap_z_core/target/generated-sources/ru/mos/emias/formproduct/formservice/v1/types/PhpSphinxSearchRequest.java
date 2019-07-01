
package ru.mos.emias.formproduct.formservice.v1.types;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import ru.mos.emias.formproduct.core.v1.Filters;


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
 *         &lt;element name="formId" type="{http://www.w3.org/2001/XMLSchema}int"/&gt;
 *         &lt;element name="fieldId" type="{http://www.w3.org/2001/XMLSchema}int"/&gt;
 *         &lt;element name="filters" type="{http://emias.mos.ru/formProduct/core/v1/}filters"/&gt;
 *         &lt;element name="query" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="formType" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
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
    "formId",
    "fieldId",
    "filters",
    "query",
    "formType"
})
@XmlRootElement(name = "phpSphinxSearchRequest")
public class PhpSphinxSearchRequest {

    protected int formId;
    protected int fieldId;
    @XmlElement(required = true, nillable = true)
    protected Filters filters;
    @XmlElement(required = true)
    protected String query;
    @XmlElement(required = true, nillable = true)
    protected String formType;

    /**
     * Gets the value of the formId property.
     * 
     */
    public int getFormId() {
        return formId;
    }

    /**
     * Sets the value of the formId property.
     * 
     */
    public void setFormId(int value) {
        this.formId = value;
    }

    /**
     * Gets the value of the fieldId property.
     * 
     */
    public int getFieldId() {
        return fieldId;
    }

    /**
     * Sets the value of the fieldId property.
     * 
     */
    public void setFieldId(int value) {
        this.fieldId = value;
    }

    /**
     * Gets the value of the filters property.
     * 
     * @return
     *     possible object is
     *     {@link Filters }
     *     
     */
    public Filters getFilters() {
        return filters;
    }

    /**
     * Sets the value of the filters property.
     * 
     * @param value
     *     allowed object is
     *     {@link Filters }
     *     
     */
    public void setFilters(Filters value) {
        this.filters = value;
    }

    /**
     * Gets the value of the query property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getQuery() {
        return query;
    }

    /**
     * Sets the value of the query property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setQuery(String value) {
        this.query = value;
    }

    /**
     * Gets the value of the formType property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getFormType() {
        return formType;
    }

    /**
     * Sets the value of the formType property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setFormType(String value) {
        this.formType = value;
    }

}
