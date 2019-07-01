
package ru.mos.emias.system.v1.faults;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * Непредвиденная ошибка
 * 
 * <p>Java class for UnexpectedFault complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="UnexpectedFault"&gt;
 *   &lt;complexContent&gt;
 *     &lt;extension base="{http://emias.mos.ru/system/v1/faults/}BaseFault"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="version" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="stackTrace"&gt;
 *           &lt;complexType&gt;
 *             &lt;complexContent&gt;
 *               &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *                 &lt;sequence&gt;
 *                   &lt;element name="stackTraceRecord" maxOccurs="10"&gt;
 *                     &lt;complexType&gt;
 *                       &lt;complexContent&gt;
 *                         &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *                           &lt;sequence&gt;
 *                             &lt;element name="declaringClass" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *                             &lt;element name="methodName" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/&gt;
 *                             &lt;element name="fileName" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/&gt;
 *                             &lt;element name="lineNumber" type="{http://www.w3.org/2001/XMLSchema}int" minOccurs="0"/&gt;
 *                           &lt;/sequence&gt;
 *                         &lt;/restriction&gt;
 *                       &lt;/complexContent&gt;
 *                     &lt;/complexType&gt;
 *                   &lt;/element&gt;
 *                 &lt;/sequence&gt;
 *               &lt;/restriction&gt;
 *             &lt;/complexContent&gt;
 *           &lt;/complexType&gt;
 *         &lt;/element&gt;
 *       &lt;/sequence&gt;
 *       &lt;attribute name="type" use="required" type="{http://emias.mos.ru/system/v1/faults/}FaultTypes" fixed="UNEXPECTED" /&gt;
 *     &lt;/extension&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "UnexpectedFault", propOrder = {
    "version",
    "stackTrace"
})
public class UnexpectedFault
    extends BaseFault
{

    @XmlElement(required = true)
    protected String version;
    @XmlElement(required = true)
    protected UnexpectedFault.StackTrace stackTrace;
    @XmlAttribute(name = "type", required = true)
    protected FaultTypes type;

    /**
     * Gets the value of the version property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getVersion() {
        return version;
    }

    /**
     * Sets the value of the version property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setVersion(String value) {
        this.version = value;
    }

    /**
     * Gets the value of the stackTrace property.
     * 
     * @return
     *     possible object is
     *     {@link UnexpectedFault.StackTrace }
     *     
     */
    public UnexpectedFault.StackTrace getStackTrace() {
        return stackTrace;
    }

    /**
     * Sets the value of the stackTrace property.
     * 
     * @param value
     *     allowed object is
     *     {@link UnexpectedFault.StackTrace }
     *     
     */
    public void setStackTrace(UnexpectedFault.StackTrace value) {
        this.stackTrace = value;
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
            return FaultTypes.UNEXPECTED;
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
     *         &lt;element name="stackTraceRecord" maxOccurs="10"&gt;
     *           &lt;complexType&gt;
     *             &lt;complexContent&gt;
     *               &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
     *                 &lt;sequence&gt;
     *                   &lt;element name="declaringClass" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
     *                   &lt;element name="methodName" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/&gt;
     *                   &lt;element name="fileName" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/&gt;
     *                   &lt;element name="lineNumber" type="{http://www.w3.org/2001/XMLSchema}int" minOccurs="0"/&gt;
     *                 &lt;/sequence&gt;
     *               &lt;/restriction&gt;
     *             &lt;/complexContent&gt;
     *           &lt;/complexType&gt;
     *         &lt;/element&gt;
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
        "stackTraceRecord"
    })
    public static class StackTrace {

        @XmlElement(required = true)
        protected List<UnexpectedFault.StackTrace.StackTraceRecord> stackTraceRecord;

        /**
         * Gets the value of the stackTraceRecord property.
         * 
         * <p>
         * This accessor method returns a reference to the live list,
         * not a snapshot. Therefore any modification you make to the
         * returned list will be present inside the JAXB object.
         * This is why there is not a <CODE>set</CODE> method for the stackTraceRecord property.
         * 
         * <p>
         * For example, to add a new item, do as follows:
         * <pre>
         *    getStackTraceRecord().add(newItem);
         * </pre>
         * 
         * 
         * <p>
         * Objects of the following type(s) are allowed in the list
         * {@link UnexpectedFault.StackTrace.StackTraceRecord }
         * 
         * 
         */
        public List<UnexpectedFault.StackTrace.StackTraceRecord> getStackTraceRecord() {
            if (stackTraceRecord == null) {
                stackTraceRecord = new ArrayList<UnexpectedFault.StackTrace.StackTraceRecord>();
            }
            return this.stackTraceRecord;
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
         *         &lt;element name="declaringClass" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
         *         &lt;element name="methodName" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/&gt;
         *         &lt;element name="fileName" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/&gt;
         *         &lt;element name="lineNumber" type="{http://www.w3.org/2001/XMLSchema}int" minOccurs="0"/&gt;
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
            "declaringClass",
            "methodName",
            "fileName",
            "lineNumber"
        })
        public static class StackTraceRecord {

            @XmlElement(required = true)
            protected String declaringClass;
            protected String methodName;
            protected String fileName;
            protected Integer lineNumber;

            /**
             * Gets the value of the declaringClass property.
             * 
             * @return
             *     possible object is
             *     {@link String }
             *     
             */
            public String getDeclaringClass() {
                return declaringClass;
            }

            /**
             * Sets the value of the declaringClass property.
             * 
             * @param value
             *     allowed object is
             *     {@link String }
             *     
             */
            public void setDeclaringClass(String value) {
                this.declaringClass = value;
            }

            /**
             * Gets the value of the methodName property.
             * 
             * @return
             *     possible object is
             *     {@link String }
             *     
             */
            public String getMethodName() {
                return methodName;
            }

            /**
             * Sets the value of the methodName property.
             * 
             * @param value
             *     allowed object is
             *     {@link String }
             *     
             */
            public void setMethodName(String value) {
                this.methodName = value;
            }

            /**
             * Gets the value of the fileName property.
             * 
             * @return
             *     possible object is
             *     {@link String }
             *     
             */
            public String getFileName() {
                return fileName;
            }

            /**
             * Sets the value of the fileName property.
             * 
             * @param value
             *     allowed object is
             *     {@link String }
             *     
             */
            public void setFileName(String value) {
                this.fileName = value;
            }

            /**
             * Gets the value of the lineNumber property.
             * 
             * @return
             *     possible object is
             *     {@link Integer }
             *     
             */
            public Integer getLineNumber() {
                return lineNumber;
            }

            /**
             * Sets the value of the lineNumber property.
             * 
             * @param value
             *     allowed object is
             *     {@link Integer }
             *     
             */
            public void setLineNumber(Integer value) {
                this.lineNumber = value;
            }

        }

    }

}
