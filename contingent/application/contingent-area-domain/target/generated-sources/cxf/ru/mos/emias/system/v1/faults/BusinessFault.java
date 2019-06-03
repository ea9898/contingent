
package ru.mos.emias.system.v1.faults;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * Бизнес ошибка
 * 
 * <p>Java class for BusinessFault complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="BusinessFault"&gt;
 *   &lt;complexContent&gt;
 *     &lt;extension base="{http://emias.mos.ru/system/v1/faults/}BaseFault"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="messages" type="{http://emias.mos.ru/system/v1/faults/}ErrorMessageCollection"/&gt;
 *         &lt;element name="hasErrors" type="{http://www.w3.org/2001/XMLSchema}boolean"/&gt;
 *       &lt;/sequence&gt;
 *       &lt;attribute name="type" use="required" type="{http://emias.mos.ru/system/v1/faults/}FaultTypes" fixed="BUSINESS" /&gt;
 *     &lt;/extension&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "BusinessFault", propOrder = {
    "messages",
    "hasErrors"
})
public class BusinessFault
    extends BaseFault
    implements Serializable
{

    private final static long serialVersionUID = 1234567890L;
    @XmlElement(required = true)
    protected ErrorMessageCollection messages;
    protected boolean hasErrors;
    @XmlAttribute(name = "type", required = true)
    protected FaultTypes type;

    /**
     * Gets the value of the messages property.
     * 
     * @return
     *     possible object is
     *     {@link ErrorMessageCollection }
     *     
     */
    public ErrorMessageCollection getMessages() {
        return messages;
    }

    /**
     * Sets the value of the messages property.
     * 
     * @param value
     *     allowed object is
     *     {@link ErrorMessageCollection }
     *     
     */
    public void setMessages(ErrorMessageCollection value) {
        this.messages = value;
    }

    /**
     * Gets the value of the hasErrors property.
     * 
     */
    public boolean isHasErrors() {
        return hasErrors;
    }

    /**
     * Sets the value of the hasErrors property.
     * 
     */
    public void setHasErrors(boolean value) {
        this.hasErrors = value;
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
            return FaultTypes.BUSINESS;
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
