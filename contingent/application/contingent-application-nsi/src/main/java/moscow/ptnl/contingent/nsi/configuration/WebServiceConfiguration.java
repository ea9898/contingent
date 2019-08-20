package moscow.ptnl.contingent.nsi.configuration;

import moscow.ptnl.contingent.nsi.ws.NsiAdminWebServiceImpl;
import org.apache.cxf.Bus;
import org.apache.cxf.bus.spring.SpringBus;
import javax.xml.ws.Endpoint;
import org.apache.cxf.jaxws.EndpointImpl;
import org.apache.cxf.ext.logging.LoggingFeature;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

import javax.xml.namespace.QName;
import java.util.ArrayList;
import java.util.Arrays;

@Configuration
@ComponentScan(basePackages = "moscow.ptnl")
public class WebServiceConfiguration  {

    @Bean
    LoggingFeature loggingFeature() {
        LoggingFeature loggingFeature = new LoggingFeature();
        loggingFeature.setLimit(-1);
        return loggingFeature;
    }

    @Bean(name = Bus.DEFAULT_BUS_ID)
    SpringBus springBus(LoggingFeature loggingFeature) {
        SpringBus bus = new SpringBus();
        bus.setFeatures(new ArrayList<>(Arrays.asList(loggingFeature)));
        return bus;
    }

    @Bean
    public Endpoint TestServiceV1(@Qualifier(moscow.ptnl.contingent.nsi.ws.NsiWebServiceImpl.SERVICE_NAME)
                                              ru.mos.emias.pushaccepterproduct.pushaccepterservice.v1.PushaccepterServicePortType pushaccepterServicePortType, SpringBus cxfBus) {
        EndpointImpl endpoint = new EndpointImpl(cxfBus, pushaccepterServicePortType);

        endpoint.setServiceName(new QName("http://emias.mos.ru/pushaccepterProduct/pushaccepterService/v1/", "getService"));
        endpoint.setWsdlLocation("classpath:META-INF/wsdl/pushaccepterProduct.pushaccepterService.v1.wsdl");
        endpoint.setAddress("/service");
        endpoint.publish(); // http://localhost:8080/nsi/service?wsdl

        //endpoint.getInInterceptors().add(soapVersionInterceptor);
//        endpoint.getInInterceptors().add(credentialsValidator());
//        interceptorService.setupInterceptors(endpoint);

        return endpoint;
    }

    @Bean
    public Endpoint adminServiceV1(@Qualifier(NsiAdminWebServiceImpl.SERVICE_NAME)
                                          ru.mos.emias.pushaccepterproduct.adminservice.v1.AdminServicePortType adminServicePortType, SpringBus cxfBus) {
        EndpointImpl endpoint = new EndpointImpl(cxfBus, adminServicePortType);
        endpoint.setServiceName(new QName("http://emias.mos.ru/pushaccepterProduct/adminService/v1/", "adminService"));
        endpoint.setWsdlLocation("classpath:META-INF/wsdl/pushaccepterProduct.adminService.v1.wsdl");
        endpoint.setAddress("/adminService");
        endpoint.publish(); // http://localhost:8080/nsi/adminService?wsdl
        return endpoint;
    }
}
