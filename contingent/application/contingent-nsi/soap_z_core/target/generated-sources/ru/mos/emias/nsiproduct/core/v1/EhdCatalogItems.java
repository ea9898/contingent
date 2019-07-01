
package ru.mos.emias.nsiproduct.core.v1;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for ehdCatalogItems complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="ehdCatalogItems"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="rows" type="{http://emias.mos.ru/nsiProduct/core/v1/}ehdCatalogRow" maxOccurs="unbounded" minOccurs="0"/&gt;
 *         &lt;element name="ehdException" type="{http://emias.mos.ru/nsiProduct/core/v1/}ehdException" maxOccurs="unbounded" minOccurs="0"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "ehdCatalogItems", propOrder = {
    "rows",
    "ehdException"
})
public class EhdCatalogItems {

    protected List<EhdCatalogRow> rows;
    protected List<EhdException> ehdException;

    /**
     * Gets the value of the rows property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the rows property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getRows().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link EhdCatalogRow }
     * 
     * 
     */
    public List<EhdCatalogRow> getRows() {
        if (rows == null) {
            rows = new ArrayList<EhdCatalogRow>();
        }
        return this.rows;
    }

    /**
     * Gets the value of the ehdException property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the ehdException property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getEhdException().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link EhdException }
     * 
     * 
     */
    public List<EhdException> getEhdException() {
        if (ehdException == null) {
            ehdException = new ArrayList<EhdException>();
        }
        return this.ehdException;
    }

}
