
package ru.mos.emias.system.v1.faults;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import ru.mos.emias.system.v1.usercontext.UserContext;


/**
 * Ошибка безопасности
 * 
 * <p>Java class for SecurityFault complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="SecurityFault"&gt;
 *   &lt;complexContent&gt;
 *     &lt;extension base="{http://emias.mos.ru/system/v1/faults/}BaseFault"&gt;
 *       &lt;sequence&gt;
 *         &lt;element ref="{http://emias.mos.ru/system/v1/userContext/}userContext"/&gt;
 *         &lt;choice&gt;
 *           &lt;element name="unauthorizedRequestSecurityException" type="{http://emias.mos.ru/system/v1/faults/}UnauthorizedRequestSecurityException"/&gt;
 *           &lt;element name="otherSecurityException" type="{http://emias.mos.ru/system/v1/faults/}OtherSecurityException"/&gt;
 *         &lt;/choice&gt;
 *       &lt;/sequence&gt;
 *       &lt;attribute name="type" use="required" type="{http://emias.mos.ru/system/v1/faults/}FaultTypes" fixed="SECURITY" /&gt;
 *     &lt;/extension&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "SecurityFault", propOrder = {
    "userContext",
    "otherSecurityException",
    "unauthorizedRequestSecurityException"
})
public class SecurityFault
    extends BaseFault
    implements Serializable
{

    private final static long serialVersionUID = 1234567890L;
    @XmlElement(namespace = "http://emias.mos.ru/system/v1/userContext/", required = true)
    protected UserContext userContext;
    protected OtherSecurityException otherSecurityException;
    protected UnauthorizedRequestSecurityException unauthorizedRequestSecurityException;
    @XmlAttribute(name = "type", required = true)
    protected FaultTypes type;

    /**
     * Gets the value of the userContext property.
     * 
     * @return
     *     possible object is
     *     {@link UserContext }
     *     
     */
    public UserContext getUserContext() {
        return userContext;
    }

    /**
     * Sets the value of the userContext property.
     * 
     * @param value
     *     allowed object is
     *     {@link UserContext }
     *     
     */
    public void setUserContext(UserContext value) {
        this.userContext = value;
    }

    /**
     * Gets the value of the otherSecurityException property.
     * 
     * @return
     *     possible object is
     *     {@link OtherSecurityException }
     *     
     */
    public OtherSecurityException getOtherSecurityException() {
        return otherSecurityException;
    }

    /**
     * Sets the value of the otherSecurityException property.
     * 
     * @param value
     *     allowed object is
     *     {@link OtherSecurityException }
     *     
     */
    public void setOtherSecurityException(OtherSecurityException value) {
        this.otherSecurityException = value;
    }

    /**
     * Gets the value of the unauthorizedRequestSecurityException property.
     * 
     * @return
     *     possible object is
     *     {@link UnauthorizedRequestSecurityException }
     *     
     */
    public UnauthorizedRequestSecurityException getUnauthorizedRequestSecurityException() {
        return unauthorizedRequestSecurityException;
    }

    /**
     * Sets the value of the unauthorizedRequestSecurityException property.
     * 
     * @param value
     *     allowed object is
     *     {@link UnauthorizedRequestSecurityException }
     *     
     */
    public void setUnauthorizedRequestSecurityException(UnauthorizedRequestSecurityException value) {
        this.unauthorizedRequestSecurityException = value;
    }

    /**
     * Gets the value of the type property.
     * 
     * @return
     *     possible object is
     *     {@link FaultTypes }
     *     
     */
    public FaultTypes getType() {
        if (type == null) {
            return FaultTypes.SECURITY;
        } else {
            return type;
        }
    }

    /**
     * Sets the value of the type property.
     * 
     * @param value
     *     allowed object is
     *     {@link FaultTypes }
     *     
     */
    public void setType(FaultTypes value) {
        this.type = value;
    }

}
