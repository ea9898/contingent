package ru.mos.emias.nsiproductpublication.publicationservice.v1;

import java.net.URL;
import javax.xml.namespace.QName;
import javax.xml.ws.WebEndpoint;
import javax.xml.ws.WebServiceClient;
import javax.xml.ws.WebServiceFeature;
import javax.xml.ws.Service;

/**
 * This class was generated by Apache CXF 3.2.7
 * 2019-07-01T17:12:17.185+03:00
 * Generated source version: 3.2.7
 *
 */
@WebServiceClient(name = "publicationService",
                  wsdlLocation = "classpath:nsiProductPublication.publicationService.v1.wsdl",
                  targetNamespace = "http://emias.mos.ru/nsiProductPublication/publicationService/v1/")
public class PublicationService extends Service {

    public final static URL WSDL_LOCATION;

    public final static QName SERVICE = new QName("http://emias.mos.ru/nsiProductPublication/publicationService/v1/", "publicationService");
    public final static QName PublicationServicePort = new QName("http://emias.mos.ru/nsiProductPublication/publicationService/v1/", "publicationServicePort");
    static {
        URL url = PublicationService.class.getClassLoader().getResource("nsiProductPublication.publicationService.v1.wsdl");
        if (url == null) {
            java.util.logging.Logger.getLogger(PublicationService.class.getName())
                .log(java.util.logging.Level.INFO,
                     "Can not initialize the default wsdl from {0}", "classpath:nsiProductPublication.publicationService.v1.wsdl");
        }
        WSDL_LOCATION = url;
    }

    public PublicationService(URL wsdlLocation) {
        super(wsdlLocation, SERVICE);
    }

    public PublicationService(URL wsdlLocation, QName serviceName) {
        super(wsdlLocation, serviceName);
    }

    public PublicationService() {
        super(WSDL_LOCATION, SERVICE);
    }

    public PublicationService(WebServiceFeature ... features) {
        super(WSDL_LOCATION, SERVICE, features);
    }

    public PublicationService(URL wsdlLocation, WebServiceFeature ... features) {
        super(wsdlLocation, SERVICE, features);
    }

    public PublicationService(URL wsdlLocation, QName serviceName, WebServiceFeature ... features) {
        super(wsdlLocation, serviceName, features);
    }




    /**
     *
     * @return
     *     returns PublicationServicePortType
     */
    @WebEndpoint(name = "publicationServicePort")
    public PublicationServicePortType getPublicationServicePort() {
        return super.getPort(PublicationServicePort, PublicationServicePortType.class);
    }

    /**
     *
     * @param features
     *     A list of {@link javax.xml.ws.WebServiceFeature} to configure on the proxy.  Supported features not in the <code>features</code> parameter will have their default values.
     * @return
     *     returns PublicationServicePortType
     */
    @WebEndpoint(name = "publicationServicePort")
    public PublicationServicePortType getPublicationServicePort(WebServiceFeature... features) {
        return super.getPort(PublicationServicePort, PublicationServicePortType.class, features);
    }

}
