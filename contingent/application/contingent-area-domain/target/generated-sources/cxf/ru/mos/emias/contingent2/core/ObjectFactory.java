
package ru.mos.emias.contingent2.core;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the ru.mos.emias.contingent2.core package. 
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

    private final static QName _MedicalOrganizationId_QNAME = new QName("http://emias.mos.ru/contingent2/core/v1/", "medicalOrganizationId");
    private final static QName _MedicalFacilityId_QNAME = new QName("http://emias.mos.ru/contingent2/core/v1/", "medicalFacilityId");
    private final static QName _OperationId_QNAME = new QName("http://emias.mos.ru/contingent2/core/v1/", "operationId");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: ru.mos.emias.contingent2.core
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link Area }
     * 
     */
    public Area createArea() {
        return new Area();
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
     * Create an instance of {@link KeyValuePair }
     * 
     */
    public KeyValuePair createKeyValuePair() {
        return new KeyValuePair();
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
     * Create an instance of {@link OperationCompletenessPercentage }
     * 
     */
    public OperationCompletenessPercentage createOperationCompletenessPercentage() {
        return new OperationCompletenessPercentage();
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
     * Create an instance of {@link AreaTypeShort }
     * 
     */
    public AreaTypeShort createAreaTypeShort() {
        return new AreaTypeShort();
    }

    /**
     * Create an instance of {@link ru.mos.emias.contingent2.core.PrimaryAreaTypeCodes }
     * 
     */
    public ru.mos.emias.contingent2.core.PrimaryAreaTypeCodes createPrimaryAreaTypeCodes() {
        return new ru.mos.emias.contingent2.core.PrimaryAreaTypeCodes();
    }

    /**
     * Create an instance of {@link AddressAllocationOrder }
     * 
     */
    public AddressAllocationOrder createAddressAllocationOrder() {
        return new AddressAllocationOrder();
    }

    /**
     * Create an instance of {@link PositionNomClinic }
     * 
     */
    public PositionNomClinic createPositionNomClinic() {
        return new PositionNomClinic();
    }

    /**
     * Create an instance of {@link MedicalEmployee }
     * 
     */
    public MedicalEmployee createMedicalEmployee() {
        return new MedicalEmployee();
    }

    /**
     * Create an instance of {@link AddMedicalEmployees }
     * 
     */
    public AddMedicalEmployees createAddMedicalEmployees() {
        return new AddMedicalEmployees();
    }

    /**
     * Create an instance of {@link AddMedicalEmployee }
     * 
     */
    public AddMedicalEmployee createAddMedicalEmployee() {
        return new AddMedicalEmployee();
    }

    /**
     * Create an instance of {@link ChangeMedicalEmployees }
     * 
     */
    public ChangeMedicalEmployees createChangeMedicalEmployees() {
        return new ChangeMedicalEmployees();
    }

    /**
     * Create an instance of {@link ChangeMedicalEmployee }
     * 
     */
    public ChangeMedicalEmployee createChangeMedicalEmployee() {
        return new ChangeMedicalEmployee();
    }

    /**
     * Create an instance of {@link NsiAddress }
     * 
     */
    public NsiAddress createNsiAddress() {
        return new NsiAddress();
    }

    /**
     * Create an instance of {@link NotNsiAddress }
     * 
     */
    public NotNsiAddress createNotNsiAddress() {
        return new NotNsiAddress();
    }

    /**
     * Create an instance of {@link Address }
     * 
     */
    public Address createAddress() {
        return new Address();
    }

    /**
     * Create an instance of {@link MoAddress }
     * 
     */
    public MoAddress createMoAddress() {
        return new MoAddress();
    }

    /**
     * Create an instance of {@link MuTypes }
     * 
     */
    public MuTypes createMuTypes() {
        return new MuTypes();
    }

    /**
     * Create an instance of {@link MuType }
     * 
     */
    public MuType createMuType() {
        return new MuType();
    }

    /**
     * Create an instance of {@link Fault }
     * 
     */
    public Fault createFault() {
        return new Fault();
    }

    /**
     * Create an instance of {@link Area.PrimaryAreaTypeCodes }
     * 
     */
    public Area.PrimaryAreaTypeCodes createAreaPrimaryAreaTypeCodes() {
        return new Area.PrimaryAreaTypeCodes();
    }

    /**
     * Create an instance of {@link Area.MedicalEmployees }
     * 
     */
    public Area.MedicalEmployees createAreaMedicalEmployees() {
        return new Area.MedicalEmployees();
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
    @XmlElementDecl(namespace = "http://emias.mos.ru/contingent2/core/v1/", name = "medicalOrganizationId")
    public JAXBElement<Long> createMedicalOrganizationId(Long value) {
        return new JAXBElement<Long>(_MedicalOrganizationId_QNAME, Long.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link Long }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://emias.mos.ru/contingent2/core/v1/", name = "medicalFacilityId")
    public JAXBElement<Long> createMedicalFacilityId(Long value) {
        return new JAXBElement<Long>(_MedicalFacilityId_QNAME, Long.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link Long }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://emias.mos.ru/contingent2/core/v1/", name = "operationId")
    public JAXBElement<Long> createOperationId(Long value) {
        return new JAXBElement<Long>(_OperationId_QNAME, Long.class, null, value);
    }

}
