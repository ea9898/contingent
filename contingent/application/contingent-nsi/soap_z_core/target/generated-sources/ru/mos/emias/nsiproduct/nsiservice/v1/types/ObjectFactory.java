
package ru.mos.emias.nsiproduct.nsiservice.v1.types;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;
import ru.mos.emias.nsiproduct.core.v1.OperationExecutionStatus;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the ru.mos.emias.nsiproduct.nsiservice.v1.types package. 
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

    private final static QName _SetDataInAsyncGetStatusResponse_QNAME = new QName("http://emias.mos.ru/nsiProduct/nsiService/v1/types/", "setDataInAsyncGetStatusResponse");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: ru.mos.emias.nsiproduct.nsiservice.v1.types
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link GetAllDictRequest }
     * 
     */
    public GetAllDictRequest createGetAllDictRequest() {
        return new GetAllDictRequest();
    }

    /**
     * Create an instance of {@link GetAllDictResponse }
     * 
     */
    public GetAllDictResponse createGetAllDictResponse() {
        return new GetAllDictResponse();
    }

    /**
     * Create an instance of {@link GetCatalogItemsRequest }
     * 
     */
    public GetCatalogItemsRequest createGetCatalogItemsRequest() {
        return new GetCatalogItemsRequest();
    }

    /**
     * Create an instance of {@link GetCatalogItemsResponse }
     * 
     */
    public GetCatalogItemsResponse createGetCatalogItemsResponse() {
        return new GetCatalogItemsResponse();
    }

    /**
     * Create an instance of {@link GetCatalogListRequest }
     * 
     */
    public GetCatalogListRequest createGetCatalogListRequest() {
        return new GetCatalogListRequest();
    }

    /**
     * Create an instance of {@link GetCatalogListResponse }
     * 
     */
    public GetCatalogListResponse createGetCatalogListResponse() {
        return new GetCatalogListResponse();
    }

    /**
     * Create an instance of {@link GetCatalogSpecRequest }
     * 
     */
    public GetCatalogSpecRequest createGetCatalogSpecRequest() {
        return new GetCatalogSpecRequest();
    }

    /**
     * Create an instance of {@link GetCatalogSpecResponse }
     * 
     */
    public GetCatalogSpecResponse createGetCatalogSpecResponse() {
        return new GetCatalogSpecResponse();
    }

    /**
     * Create an instance of {@link GetCatalogStatsRequest }
     * 
     */
    public GetCatalogStatsRequest createGetCatalogStatsRequest() {
        return new GetCatalogStatsRequest();
    }

    /**
     * Create an instance of {@link GetCatalogStatsResponse }
     * 
     */
    public GetCatalogStatsResponse createGetCatalogStatsResponse() {
        return new GetCatalogStatsResponse();
    }

    /**
     * Create an instance of {@link GetDictItemRequest }
     * 
     */
    public GetDictItemRequest createGetDictItemRequest() {
        return new GetDictItemRequest();
    }

    /**
     * Create an instance of {@link GetDictItemResponse }
     * 
     */
    public GetDictItemResponse createGetDictItemResponse() {
        return new GetDictItemResponse();
    }

    /**
     * Create an instance of {@link SetDataInRequest }
     * 
     */
    public SetDataInRequest createSetDataInRequest() {
        return new SetDataInRequest();
    }

    /**
     * Create an instance of {@link SetDataInResponse }
     * 
     */
    public SetDataInResponse createSetDataInResponse() {
        return new SetDataInResponse();
    }

    /**
     * Create an instance of {@link SetDataInAsyncRequest }
     * 
     */
    public SetDataInAsyncRequest createSetDataInAsyncRequest() {
        return new SetDataInAsyncRequest();
    }

    /**
     * Create an instance of {@link SetDataInAsyncResponse }
     * 
     */
    public SetDataInAsyncResponse createSetDataInAsyncResponse() {
        return new SetDataInAsyncResponse();
    }

    /**
     * Create an instance of {@link SetDataInAsyncGetStatusRequest }
     * 
     */
    public SetDataInAsyncGetStatusRequest createSetDataInAsyncGetStatusRequest() {
        return new SetDataInAsyncGetStatusRequest();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link OperationExecutionStatus }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://emias.mos.ru/nsiProduct/nsiService/v1/types/", name = "setDataInAsyncGetStatusResponse")
    public JAXBElement<OperationExecutionStatus> createSetDataInAsyncGetStatusResponse(OperationExecutionStatus value) {
        return new JAXBElement<OperationExecutionStatus>(_SetDataInAsyncGetStatusResponse_QNAME, OperationExecutionStatus.class, null, value);
    }

}
