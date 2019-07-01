
package ru.mos.emias.nsiproduct.core.v1;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the ru.mos.emias.nsiproduct.core.v1 package. 
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

    private final static QName _Options_QNAME = new QName("http://emias.mos.ru/nsiProduct/core/v1/", "options");
    private final static QName _PagingOptions_QNAME = new QName("http://emias.mos.ru/nsiProduct/core/v1/", "pagingOptions");
    private final static QName _OperationId_QNAME = new QName("http://emias.mos.ru/nsiProduct/core/v1/", "operationId");
    private final static QName _MedicalOrganizationId_QNAME = new QName("http://emias.mos.ru/nsiProduct/core/v1/", "medicalOrganizationId");
    private final static QName _MedicalFacilityId_QNAME = new QName("http://emias.mos.ru/nsiProduct/core/v1/", "medicalFacilityId");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: ru.mos.emias.nsiproduct.core.v1
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
     * Create an instance of {@link EhdDictionaries }
     * 
     */
    public EhdDictionaries createEhdDictionaries() {
        return new EhdDictionaries();
    }

    /**
     * Create an instance of {@link PagingOptions }
     * 
     */
    public PagingOptions createPagingOptions() {
        return new PagingOptions();
    }

    /**
     * Create an instance of {@link EhdCatalogItems }
     * 
     */
    public EhdCatalogItems createEhdCatalogItems() {
        return new EhdCatalogItems();
    }

    /**
     * Create an instance of {@link EhdCatalogs }
     * 
     */
    public EhdCatalogs createEhdCatalogs() {
        return new EhdCatalogs();
    }

    /**
     * Create an instance of {@link EhdAttrSpec }
     * 
     */
    public EhdAttrSpec createEhdAttrSpec() {
        return new EhdAttrSpec();
    }

    /**
     * Create an instance of {@link EhdCatalogStats }
     * 
     */
    public EhdCatalogStats createEhdCatalogStats() {
        return new EhdCatalogStats();
    }

    /**
     * Create an instance of {@link EhdDictionaryItems }
     * 
     */
    public EhdDictionaryItems createEhdDictionaryItems() {
        return new EhdDictionaryItems();
    }

    /**
     * Create an instance of {@link DataInMessage }
     * 
     */
    public DataInMessage createDataInMessage() {
        return new DataInMessage();
    }

    /**
     * Create an instance of {@link ListResultInfo }
     * 
     */
    public ListResultInfo createListResultInfo() {
        return new ListResultInfo();
    }

    /**
     * Create an instance of {@link OperationExecutionStatus }
     * 
     */
    public OperationExecutionStatus createOperationExecutionStatus() {
        return new OperationExecutionStatus();
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
     * Create an instance of {@link EhdDictionary }
     * 
     */
    public EhdDictionary createEhdDictionary() {
        return new EhdDictionary();
    }

    /**
     * Create an instance of {@link EhdDictionaryItem }
     * 
     */
    public EhdDictionaryItem createEhdDictionaryItem() {
        return new EhdDictionaryItem();
    }

    /**
     * Create an instance of {@link EhdDictionaryItemsV2 }
     * 
     */
    public EhdDictionaryItemsV2 createEhdDictionaryItemsV2() {
        return new EhdDictionaryItemsV2();
    }

    /**
     * Create an instance of {@link EhdDictionaryItemV2 }
     * 
     */
    public EhdDictionaryItemV2 createEhdDictionaryItemV2() {
        return new EhdDictionaryItemV2();
    }

    /**
     * Create an instance of {@link DictAttrV2 }
     * 
     */
    public DictAttrV2 createDictAttrV2() {
        return new DictAttrV2();
    }

    /**
     * Create an instance of {@link EhdCatalog }
     * 
     */
    public EhdCatalog createEhdCatalog() {
        return new EhdCatalog();
    }

    /**
     * Create an instance of {@link EhdCommonAttribute }
     * 
     */
    public EhdCommonAttribute createEhdCommonAttribute() {
        return new EhdCommonAttribute();
    }

    /**
     * Create an instance of {@link EhdException }
     * 
     */
    public EhdException createEhdException() {
        return new EhdException();
    }

    /**
     * Create an instance of {@link EhdAttrSpecNew }
     * 
     */
    public EhdAttrSpecNew createEhdAttrSpecNew() {
        return new EhdAttrSpecNew();
    }

    /**
     * Create an instance of {@link EhdCommonAttributeNew }
     * 
     */
    public EhdCommonAttributeNew createEhdCommonAttributeNew() {
        return new EhdCommonAttributeNew();
    }

    /**
     * Create an instance of {@link EhdCatalogRow }
     * 
     */
    public EhdCatalogRow createEhdCatalogRow() {
        return new EhdCatalogRow();
    }

    /**
     * Create an instance of {@link EhdCatalogItem }
     * 
     */
    public EhdCatalogItem createEhdCatalogItem() {
        return new EhdCatalogItem();
    }

    /**
     * Create an instance of {@link ListResultUid }
     * 
     */
    public ListResultUid createListResultUid() {
        return new ListResultUid();
    }

    /**
     * Create an instance of {@link ResultUid }
     * 
     */
    public ResultUid createResultUid() {
        return new ResultUid();
    }

    /**
     * Create an instance of {@link Catalog }
     * 
     */
    public Catalog createCatalog() {
        return new Catalog();
    }

    /**
     * Create an instance of {@link Items }
     * 
     */
    public Items createItems() {
        return new Items();
    }

    /**
     * Create an instance of {@link Item }
     * 
     */
    public Item createItem() {
        return new Item();
    }

    /**
     * Create an instance of {@link Data }
     * 
     */
    public Data createData() {
        return new Data();
    }

    /**
     * Create an instance of {@link Attribute }
     * 
     */
    public Attribute createAttribute() {
        return new Attribute();
    }

    /**
     * Create an instance of {@link Values }
     * 
     */
    public Values createValues() {
        return new Values();
    }

    /**
     * Create an instance of {@link Value }
     * 
     */
    public Value createValue() {
        return new Value();
    }

    /**
     * Create an instance of {@link Files }
     * 
     */
    public Files createFiles() {
        return new Files();
    }

    /**
     * Create an instance of {@link File }
     * 
     */
    public File createFile() {
        return new File();
    }

    /**
     * Create an instance of {@link ResultInfo }
     * 
     */
    public ResultInfo createResultInfo() {
        return new ResultInfo();
    }

    /**
     * Create an instance of {@link ErrorMessage.Parameters }
     * 
     */
    public ErrorMessage.Parameters createErrorMessageParameters() {
        return new ErrorMessage.Parameters();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link Options }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://emias.mos.ru/nsiProduct/core/v1/", name = "options")
    public JAXBElement<Options> createOptions(Options value) {
        return new JAXBElement<Options>(_Options_QNAME, Options.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link PagingOptions }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://emias.mos.ru/nsiProduct/core/v1/", name = "pagingOptions")
    public JAXBElement<PagingOptions> createPagingOptions(PagingOptions value) {
        return new JAXBElement<PagingOptions>(_PagingOptions_QNAME, PagingOptions.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link Long }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://emias.mos.ru/nsiProduct/core/v1/", name = "operationId")
    public JAXBElement<Long> createOperationId(Long value) {
        return new JAXBElement<Long>(_OperationId_QNAME, Long.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link Long }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://emias.mos.ru/nsiProduct/core/v1/", name = "medicalOrganizationId")
    public JAXBElement<Long> createMedicalOrganizationId(Long value) {
        return new JAXBElement<Long>(_MedicalOrganizationId_QNAME, Long.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link Long }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://emias.mos.ru/nsiProduct/core/v1/", name = "medicalFacilityId")
    public JAXBElement<Long> createMedicalFacilityId(Long value) {
        return new JAXBElement<Long>(_MedicalFacilityId_QNAME, Long.class, null, value);
    }

}
