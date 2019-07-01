
package ru.mos.emias.nsiproduct.nsiservice.v1.types;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
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
 *         &lt;element ref="{http://emias.mos.ru/nsiProduct/core/v1/}operationId"/&gt;
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
    "operationId"
})
@XmlRootElement(name = "setDataInAsyncResponse")
public class SetDataInAsyncResponse {

    @XmlElement(namespace = "http://emias.mos.ru/nsiProduct/core/v1/")
    protected long operationId;

    /**
     * Gets the value of the operationId property.
     * 
     */
    public long getOperationId() {
        return operationId;
    }

    /**
     * Sets the value of the operationId property.
     * 
     */
    public void setOperationId(long value) {
        this.operationId = value;
    }

}
