
package ru.mos.emias.contingent2.core;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * Статус выполнения операции
 * 
 * <p>Java class for OperationExecutionStatus complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="OperationExecutionStatus"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="isCompleted" type="{http://www.w3.org/2001/XMLSchema}boolean"/&gt;
 *         &lt;element name="completenessProgress" type="{http://emias.mos.ru/contingent2/core/v1/}OperationCompletenessBase"/&gt;
 *         &lt;element name="hasSucceded" type="{http://www.w3.org/2001/XMLSchema}boolean" minOccurs="0"/&gt;
 *         &lt;element name="messages" type="{http://emias.mos.ru/contingent2/core/v1/}ErrorMessageCollection" minOccurs="0"/&gt;
 *       &lt;/sequence&gt;
 *       &lt;attribute name="id" use="required" type="{http://emias.mos.ru/contingent2/core/v1/}ID" /&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "OperationExecutionStatus", propOrder = {
    "isCompleted",
    "completenessProgress",
    "hasSucceded",
    "messages"
})
public class OperationExecutionStatus
    implements Serializable
{

    private final static long serialVersionUID = 1234567890L;
    protected boolean isCompleted;
    @XmlElement(required = true)
    protected OperationCompletenessBase completenessProgress;
    protected Boolean hasSucceded;
    protected ErrorMessageCollection messages;
    @XmlAttribute(name = "id", required = true)
    protected long id;

    /**
     * Gets the value of the isCompleted property.
     * 
     */
    public boolean isIsCompleted() {
        return isCompleted;
    }

    /**
     * Sets the value of the isCompleted property.
     * 
     */
    public void setIsCompleted(boolean value) {
        this.isCompleted = value;
    }

    /**
     * Gets the value of the completenessProgress property.
     * 
     * @return
     *     possible object is
     *     {@link OperationCompletenessBase }
     *     
     */
    public OperationCompletenessBase getCompletenessProgress() {
        return completenessProgress;
    }

    /**
     * Sets the value of the completenessProgress property.
     * 
     * @param value
     *     allowed object is
     *     {@link OperationCompletenessBase }
     *     
     */
    public void setCompletenessProgress(OperationCompletenessBase value) {
        this.completenessProgress = value;
    }

    /**
     * Gets the value of the hasSucceded property.
     * 
     * @return
     *     possible object is
     *     {@link Boolean }
     *     
     */
    public Boolean isHasSucceded() {
        return hasSucceded;
    }

    /**
     * Sets the value of the hasSucceded property.
     * 
     * @param value
     *     allowed object is
     *     {@link Boolean }
     *     
     */
    public void setHasSucceded(Boolean value) {
        this.hasSucceded = value;
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
     * Gets the value of the id property.
     * 
     */
    public long getId() {
        return id;
    }

    /**
     * Sets the value of the id property.
     * 
     */
    public void setId(long value) {
        this.id = value;
    }

}
