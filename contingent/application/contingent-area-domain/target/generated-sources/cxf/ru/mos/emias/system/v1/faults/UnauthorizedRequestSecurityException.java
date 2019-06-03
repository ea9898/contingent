
package ru.mos.emias.system.v1.faults;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * Исключение безопасности типа UNAUTHORIZED_REQUEST_EXCEPTION
 * 
 * <p>Java class for UnauthorizedRequestSecurityException complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="UnauthorizedRequestSecurityException"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="requiredRights"&gt;
 *           &lt;complexType&gt;
 *             &lt;complexContent&gt;
 *               &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *                 &lt;sequence&gt;
 *                   &lt;element name="userRightId" type="{http://emias.mos.ru/system/v1/faults/}ID" maxOccurs="unbounded"/&gt;
 *                 &lt;/sequence&gt;
 *               &lt;/restriction&gt;
 *             &lt;/complexContent&gt;
 *           &lt;/complexType&gt;
 *         &lt;/element&gt;
 *       &lt;/sequence&gt;
 *       &lt;attribute name="type" use="required" type="{http://emias.mos.ru/system/v1/faults/}SecurityExceptionTypes" fixed="UNAUTHORIZED_REQUEST_EXCEPTION" /&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "UnauthorizedRequestSecurityException", propOrder = {
    "requiredRights"
})
public class UnauthorizedRequestSecurityException
    implements Serializable
{

    private final static long serialVersionUID = 1234567890L;
    @XmlElement(required = true)
    protected UnauthorizedRequestSecurityException.RequiredRights requiredRights;
    @XmlAttribute(name = "type", required = true)
    protected SecurityExceptionTypes type;

    /**
     * Gets the value of the requiredRights property.
     * 
     * @return
     *     possible object is
     *     {@link UnauthorizedRequestSecurityException.RequiredRights }
     *     
     */
    public UnauthorizedRequestSecurityException.RequiredRights getRequiredRights() {
        return requiredRights;
    }

    /**
     * Sets the value of the requiredRights property.
     * 
     * @param value
     *     allowed object is
     *     {@link UnauthorizedRequestSecurityException.RequiredRights }
     *     
     */
    public void setRequiredRights(UnauthorizedRequestSecurityException.RequiredRights value) {
        this.requiredRights = value;
    }

    /**
     * Gets the value of the type property.
     * 
     * @return
     *     possible object is
     *     {@link SecurityExceptionTypes }
     *     
     */
    public SecurityExceptionTypes getType() {
        if (type == null) {
            return SecurityExceptionTypes.UNAUTHORIZED_REQUEST_EXCEPTION;
        } else {
            return type;
        }
    }

    /**
     * Sets the value of the type property.
     * 
     * @param value
     *     allowed object is
     *     {@link SecurityExceptionTypes }
     *     
     */
    public void setType(SecurityExceptionTypes value) {
        this.type = value;
    }


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
     *         &lt;element name="userRightId" type="{http://emias.mos.ru/system/v1/faults/}ID" maxOccurs="unbounded"/&gt;
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
        "userRightIds"
    })
    public static class RequiredRights
        implements Serializable
    {

        private final static long serialVersionUID = 1234567890L;
        @XmlElement(name = "userRightId", type = Long.class)
        protected List<Long> userRightIds;

        /**
         * Gets the value of the userRightIds property.
         * 
         * <p>
         * This accessor method returns a reference to the live list,
         * not a snapshot. Therefore any modification you make to the
         * returned list will be present inside the JAXB object.
         * This is why there is not a <CODE>set</CODE> method for the userRightIds property.
         * 
         * <p>
         * For example, to add a new item, do as follows:
         * <pre>
         *    getUserRightIds().add(newItem);
         * </pre>
         * 
         * 
         * <p>
         * Objects of the following type(s) are allowed in the list
         * {@link Long }
         * 
         * 
         */
        public List<Long> getUserRightIds() {
            if (userRightIds == null) {
                userRightIds = new ArrayList<Long>();
            }
            return this.userRightIds;
        }

    }

}
