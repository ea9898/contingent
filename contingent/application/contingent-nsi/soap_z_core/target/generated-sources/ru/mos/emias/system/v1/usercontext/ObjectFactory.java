
package ru.mos.emias.system.v1.usercontext;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the ru.mos.emias.system.v1.usercontext package. 
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

    private final static QName _UserContext_QNAME = new QName("http://emias.mos.ru/system/v1/userContext/", "userContext");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: ru.mos.emias.system.v1.usercontext
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link ErrorMessage }
     * 
     */
    public ErrorMessage createErrorMessage() {
        return new ErrorMessage();
    }

    /**
     * Create an instance of {@link UserContext }
     * 
     */
    public UserContext createUserContext() {
        return new UserContext();
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
     * Create an instance of {@link ErrorMessage.Parameters }
     * 
     */
    public ErrorMessage.Parameters createErrorMessageParameters() {
        return new ErrorMessage.Parameters();
    }

    /**
     * Create an instance of {@link UserContext.UserRights }
     * 
     */
    public UserContext.UserRights createUserContextUserRights() {
        return new UserContext.UserRights();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link UserContext }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://emias.mos.ru/system/v1/userContext/", name = "userContext")
    public JAXBElement<UserContext> createUserContext(UserContext value) {
        return new JAXBElement<UserContext>(_UserContext_QNAME, UserContext.class, null, value);
    }

}
