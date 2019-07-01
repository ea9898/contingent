
package ru.mos.emias.formproduct.core.v1;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the ru.mos.emias.formproduct.core.v1 package. 
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

    private final static QName _MedicalOrganizationId_QNAME = new QName("http://emias.mos.ru/formProduct/core/v1/", "medicalOrganizationId");
    private final static QName _MedicalFacilityId_QNAME = new QName("http://emias.mos.ru/formProduct/core/v1/", "medicalFacilityId");
    private final static QName _OperationId_QNAME = new QName("http://emias.mos.ru/formProduct/core/v1/", "operationId");
    private final static QName _Options_QNAME = new QName("http://emias.mos.ru/formProduct/core/v1/", "options");
    private final static QName _PagingOptions_QNAME = new QName("http://emias.mos.ru/formProduct/core/v1/", "pagingOptions");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: ru.mos.emias.formproduct.core.v1
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
     * Create an instance of {@link Options }
     * 
     */
    public Options createOptions() {
        return new Options();
    }

    /**
     * Create an instance of {@link PagingOptions }
     * 
     */
    public PagingOptions createPagingOptions() {
        return new PagingOptions();
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
     * Create an instance of {@link OperationExecutionStatus }
     * 
     */
    public OperationExecutionStatus createOperationExecutionStatus() {
        return new OperationExecutionStatus();
    }

    /**
     * Create an instance of {@link TimeInterval }
     * 
     */
    public TimeInterval createTimeInterval() {
        return new TimeInterval();
    }

    /**
     * Create an instance of {@link DatePeriod }
     * 
     */
    public DatePeriod createDatePeriod() {
        return new DatePeriod();
    }

    /**
     * Create an instance of {@link Forms }
     * 
     */
    public Forms createForms() {
        return new Forms();
    }

    /**
     * Create an instance of {@link Form }
     * 
     */
    public Form createForm() {
        return new Form();
    }

    /**
     * Create an instance of {@link Fields }
     * 
     */
    public Fields createFields() {
        return new Fields();
    }

    /**
     * Create an instance of {@link Field }
     * 
     */
    public Field createField() {
        return new Field();
    }

    /**
     * Create an instance of {@link Filters }
     * 
     */
    public Filters createFilters() {
        return new Filters();
    }

    /**
     * Create an instance of {@link Filter }
     * 
     */
    public Filter createFilter() {
        return new Filter();
    }

    /**
     * Create an instance of {@link ErrorMessage.Parameters }
     * 
     */
    public ErrorMessage.Parameters createErrorMessageParameters() {
        return new ErrorMessage.Parameters();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link Long }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://emias.mos.ru/formProduct/core/v1/", name = "medicalOrganizationId")
    public JAXBElement<Long> createMedicalOrganizationId(Long value) {
        return new JAXBElement<Long>(_MedicalOrganizationId_QNAME, Long.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link Long }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://emias.mos.ru/formProduct/core/v1/", name = "medicalFacilityId")
    public JAXBElement<Long> createMedicalFacilityId(Long value) {
        return new JAXBElement<Long>(_MedicalFacilityId_QNAME, Long.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link Long }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://emias.mos.ru/formProduct/core/v1/", name = "operationId")
    public JAXBElement<Long> createOperationId(Long value) {
        return new JAXBElement<Long>(_OperationId_QNAME, Long.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link Options }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://emias.mos.ru/formProduct/core/v1/", name = "options")
    public JAXBElement<Options> createOptions(Options value) {
        return new JAXBElement<Options>(_Options_QNAME, Options.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link PagingOptions }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://emias.mos.ru/formProduct/core/v1/", name = "pagingOptions")
    public JAXBElement<PagingOptions> createPagingOptions(PagingOptions value) {
        return new JAXBElement<PagingOptions>(_PagingOptions_QNAME, PagingOptions.class, null, value);
    }

}
