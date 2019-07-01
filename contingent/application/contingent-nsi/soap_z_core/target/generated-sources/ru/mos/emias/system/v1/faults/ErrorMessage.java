
package ru.mos.emias.system.v1.faults;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * Сообщение об ошибке
 * 
 * <p>Java class for ErrorMessage complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="ErrorMessage"&gt;
 *   &lt;complexContent&gt;
 *     &lt;extension base="{http://emias.mos.ru/system/v1/faults/}Message"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="parameters" minOccurs="0"&gt;
 *           &lt;complexType&gt;
 *             &lt;complexContent&gt;
 *               &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *                 &lt;sequence&gt;
 *                   &lt;element name="parameter" type="{http://emias.mos.ru/system/v1/faults/}KeyValuePair" maxOccurs="unbounded"/&gt;
 *                 &lt;/sequence&gt;
 *               &lt;/restriction&gt;
 *             &lt;/complexContent&gt;
 *           &lt;/complexType&gt;
 *         &lt;/element&gt;
 *         &lt;element name="messages" type="{http://emias.mos.ru/system/v1/faults/}ErrorMessageCollection" minOccurs="0"/&gt;
 *       &lt;/sequence&gt;
 *       &lt;attribute name="type" use="required" type="{http://emias.mos.ru/system/v1/faults/}ErrorMessageTypes" /&gt;
 *     &lt;/extension&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "ErrorMessage", propOrder = {
    "parameters",
    "messages"
})
public class ErrorMessage
    extends Message
{

    protected ErrorMessage.Parameters parameters;
    protected ErrorMessageCollection messages;
    @XmlAttribute(name = "type", required = true)
    protected ErrorMessageTypes type;

    /**
     * Gets the value of the parameters property.
     * 
     * @return
     *     possible object is
     *     {@link ErrorMessage.Parameters }
     *     
     */
    public ErrorMessage.Parameters getParameters() {
        return parameters;
    }

    /**
     * Sets the value of the parameters property.
     * 
     * @param value
     *     allowed object is
     *     {@link ErrorMessage.Parameters }
     *     
     */
    public void setParameters(ErrorMessage.Parameters value) {
        this.parameters = value;
    }

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
     * Gets the value of the type property.
     * 
     * @return
     *     possible object is
     *     {@link ErrorMessageTypes }
     *     
     */
    public ErrorMessageTypes getType() {
        return type;
    }

    /**
     * Sets the value of the type property.
     * 
     * @param value
     *     allowed object is
     *     {@link ErrorMessageTypes }
     *     
     */
    public void setType(ErrorMessageTypes value) {
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
     *         &lt;element name="parameter" type="{http://emias.mos.ru/system/v1/faults/}KeyValuePair" maxOccurs="unbounded"/&gt;
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
        "parameter"
    })
    public static class Parameters {

        @XmlElement(required = true)
        protected List<KeyValuePair> parameter;

        /**
         * Gets the value of the parameter property.
         * 
         * <p>
         * This accessor method returns a reference to the live list,
         * not a snapshot. Therefore any modification you make to the
         * returned list will be present inside the JAXB object.
         * This is why there is not a <CODE>set</CODE> method for the parameter property.
         * 
         * <p>
         * For example, to add a new item, do as follows:
         * <pre>
         *    getParameter().add(newItem);
         * </pre>
         * 
         * 
         * <p>
         * Objects of the following type(s) are allowed in the list
         * {@link KeyValuePair }
         * 
         * 
         */
        public List<KeyValuePair> getParameter() {
            if (parameter == null) {
                parameter = new ArrayList<KeyValuePair>();
            }
            return this.parameter;
        }

    }

}
