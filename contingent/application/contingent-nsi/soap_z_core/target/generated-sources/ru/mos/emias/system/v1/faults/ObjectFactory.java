
package ru.mos.emias.system.v1.faults;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the ru.mos.emias.system.v1.faults package. 
 * <p>An ObjectFactory allows you to programatically 
 * construct new instances of the Java representation 
 * for XML content. The Java representation of XML 
 * content can consist of schema derived interfaces 
 * and classes representing the binding of schema 
 * type definitions, element declarations and model 
 * groups.  Factory methods for each of these are 
 * provided in this class.
 * 
 */
@XmlRegistry
public class ObjectFactory {

    private final static QName _Fault_QNAME = new QName("http://emias.mos.ru/system/v1/faults/", "fault");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: ru.mos.emias.system.v1.faults
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link UnauthorizedRequestSecurityException }
     * 
     */
    public UnauthorizedRequestSecurityException createUnauthorizedRequestSecurityException() {
        return new UnauthorizedRequestSecurityException();
    }

    /**
     * Create an instance of {@link UnexpectedFault }
     * 
     */
    public UnexpectedFault createUnexpectedFault() {
        return new UnexpectedFault();
    }

    /**
     * Create an instance of {@link UnexpectedFault.StackTrace }
     * 
     */
    public UnexpectedFault.StackTrace createUnexpectedFaultStackTrace() {
        return new UnexpectedFault.StackTrace();
    }

    /**
     * Create an instance of {@link ErrorMessage }
     * 
     */
    public ErrorMessage createErrorMessage() {
        return new ErrorMessage();
    }

    /**
     * Create an instance of {@link ErrorMessageCollection }
     * 
     */
    public ErrorMessageCollection createErrorMessageCollection() {
        return new ErrorMessageCollection();
    }

    /**
     * Create an instance of {@link KeyValuePair }
     * 
     */
    public KeyValuePair createKeyValuePair() {
        return new KeyValuePair();
    }

    /**
     * Create an instance of {@link Message }
     * 
     */
    public Message createMessage() {
        return new Message();
    }

    /**
     * Create an instance of {@link SecurityFault }
     * 
     */
    public SecurityFault createSecurityFault() {
        return new SecurityFault();
    }

    /**
     * Create an instance of {@link BusinessFault }
     * 
     */
    public BusinessFault createBusinessFault() {
        return new BusinessFault();
    }

    /**
     * Create an instance of {@link OtherSecurityException }
     * 
     */
    public OtherSecurityException createOtherSecurityException() {
        return new OtherSecurityException();
    }

    /**
     * Create an instance of {@link UnauthorizedRequestSecurityException.RequiredRights }
     * 
     */
    public UnauthorizedRequestSecurityException.RequiredRights createUnauthorizedRequestSecurityExceptionRequiredRights() {
        return new UnauthorizedRequestSecurityException.RequiredRights();
    }

    /**
     * Create an instance of {@link UnexpectedFault.StackTrace.StackTraceRecord }
     * 
     */
    public UnexpectedFault.StackTrace.StackTraceRecord createUnexpectedFaultStackTraceStackTraceRecord() {
        return new UnexpectedFault.StackTrace.StackTraceRecord();
    }

    /**
     * Create an instance of {@link ErrorMessage.Parameters }
     * 
     */
    public ErrorMessage.Parameters createErrorMessageParameters() {
        return new ErrorMessage.Parameters();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link BaseFault }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://emias.mos.ru/system/v1/faults/", name = "fault")
    public JAXBElement<BaseFault> createFault(BaseFault value) {
        return new JAXBElement<BaseFault>(_Fault_QNAME, BaseFault.class, null, value);
    }

}
