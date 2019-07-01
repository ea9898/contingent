
package ru.mos.emias.formproduct.formservice.v1.types;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;


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
    "fieldId"
})
@XmlRootElement(name = "phpSphinxSearchFromGlobalIdXsdRequest")
public class PhpSphinxSearchFromGlobalIdXsdRequest {

    protected int formId;
    protected int fieldId;

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

}
