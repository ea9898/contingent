
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
 *         &lt;element name="searchForm" type="{http://www.w3.org/2001/XMLSchema}int"/&gt;
 *         &lt;element name="globalId" type="{http://www.w3.org/2001/XMLSchema}int"/&gt;
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
    "searchForm",
    "globalId"
})
@XmlRootElement(name = "subscribeFormRequest")
public class SubscribeFormRequest {

    protected int searchForm;
    protected int globalId;

    /**
     * Gets the value of the searchForm property.
     * 
     */
    public int getSearchForm() {
        return searchForm;
    }

    /**
     * Sets the value of the searchForm property.
     * 
     */
    public void setSearchForm(int value) {
        this.searchForm = value;
    }

    /**
     * Gets the value of the globalId property.
     * 
     */
    public int getGlobalId() {
        return globalId;
    }

    /**
     * Sets the value of the globalId property.
     * 
     */
    public void setGlobalId(int value) {
        this.globalId = value;
    }

}
