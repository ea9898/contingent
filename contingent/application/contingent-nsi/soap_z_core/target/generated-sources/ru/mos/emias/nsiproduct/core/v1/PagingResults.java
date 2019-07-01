
package ru.mos.emias.nsiproduct.core.v1;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlSeeAlso;
import javax.xml.bind.annotation.XmlType;
import ru.mos.emias.nsiproduct.nsiservice.v1.types.GetCatalogItemsResponse;
import ru.mos.emias.nsiproduct.nsiservice.v1.types.GetDictItemResponse;


/**
 * Список с пагинацией
 * 
 * <p>Java class for PagingResults complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="PagingResults"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;attribute name="pageNumber" use="required" type="{http://www.w3.org/2001/XMLSchema}int" /&gt;
 *       &lt;attribute name="pageTotal" use="required" type="{http://www.w3.org/2001/XMLSchema}int" /&gt;
 *       &lt;attribute name="pageSize" use="required" type="{http://www.w3.org/2001/XMLSchema}int" /&gt;
 *       &lt;attribute name="morePagesAvailable" use="required" type="{http://www.w3.org/2001/XMLSchema}boolean" /&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "PagingResults")
@XmlSeeAlso({
    GetDictItemResponse.class,
    GetCatalogItemsResponse.class
})
public abstract class PagingResults {

    @XmlAttribute(name = "pageNumber", required = true)
    protected int pageNumber;
    @XmlAttribute(name = "pageTotal", required = true)
    protected int pageTotal;
    @XmlAttribute(name = "pageSize", required = true)
    protected int pageSize;
    @XmlAttribute(name = "morePagesAvailable", required = true)
    protected boolean morePagesAvailable;

    /**
     * Gets the value of the pageNumber property.
     * 
     */
    public int getPageNumber() {
        return pageNumber;
    }

    /**
     * Sets the value of the pageNumber property.
     * 
     */
    public void setPageNumber(int value) {
        this.pageNumber = value;
    }

    /**
     * Gets the value of the pageTotal property.
     * 
     */
    public int getPageTotal() {
        return pageTotal;
    }

    /**
     * Sets the value of the pageTotal property.
     * 
     */
    public void setPageTotal(int value) {
        this.pageTotal = value;
    }

    /**
     * Gets the value of the pageSize property.
     * 
     */
    public int getPageSize() {
        return pageSize;
    }

    /**
     * Sets the value of the pageSize property.
     * 
     */
    public void setPageSize(int value) {
        this.pageSize = value;
    }

    /**
     * Gets the value of the morePagesAvailable property.
     * 
     */
    public boolean isMorePagesAvailable() {
        return morePagesAvailable;
    }

    /**
     * Sets the value of the morePagesAvailable property.
     * 
     */
    public void setMorePagesAvailable(boolean value) {
        this.morePagesAvailable = value;
    }

}
