
package ru.mos.emias.contingent2.core.v2;

import java.io.Serializable;
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
 *     &lt;extension base="{http://emias.mos.ru/contingent2/core/v2/}Message"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="parameters" minOccurs="0"&gt;
 *           &lt;complexType&gt;
 *             &lt;complexContent&gt;
 *               &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *                 &lt;sequence&gt;
 *                   &lt;element name="parameter" type="{http://emias.mos.ru/contingent2/core/v2/}KeyValuePair" maxOccurs="unbounded"/&gt;
 *                 &lt;/sequence&gt;
 *               &lt;/restriction&gt;
 *             &lt;/complexContent&gt;
 *           &lt;/complexType&gt;
 *         &lt;/element&gt;
 *         &lt;element name="messages" type="{http://emias.mos.ru/contingent2/core/v2/}ErrorMessageCollection" minOccurs="0"/&gt;
 *       &lt;/sequence&gt;
 *       &lt;attribute name="type" use="required" type="{http://emias.mos.ru/contingent2/core/v2/}ErrorMessageTypes" /&gt;
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
    implements Serializable
{

    private final static long serialVersionUID = 1234567890L;
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
     *         &lt;element name="parameter" type="{http://emias.mos.ru/contingent2/core/v2/}KeyValuePair" maxOccurs="unbounded"/&gt;
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
        "parameters"
    })
    public static class Parameters
        implements Serializable
    {

        private final static long serialVersionUID = 1234567890L;
        @XmlElement(name = "parameter", required = true)
        protected List<KeyValuePair> parameters;

        /**
         * Gets the value of the parameters property.
         * 
         * <p>
         * This accessor method returns a reference to the live list,
         * not a snapshot. Therefore any modification you make to the
         * returned list will be present inside the JAXB object.
         * This is why there is not a <CODE>set</CODE> method for the parameters property.
         * 
         * <p>
         * For example, to add a new item, do as follows:
         * <pre>
         *    getParameters().add(newItem);
         * </pre>
         * 
         * 
         * <p>
         * Objects of the following type(s) are allowed in the list
         * {@link KeyValuePair }
         * 
         * 
         */
        public List<KeyValuePair> getParameters() {
            if (parameters == null) {
                parameters = new ArrayList<KeyValuePair>();
            }
            return this.parameters;
        }

    }

}