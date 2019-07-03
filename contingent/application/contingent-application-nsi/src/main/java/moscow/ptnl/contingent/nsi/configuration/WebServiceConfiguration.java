package moscow.ptnl.contingent.nsi.configuration;

import org.apache.cxf.Bus;
import org.apache.cxf.bus.spring.SpringBus;
import javax.xml.ws.Endpoint;
import org.apache.cxf.jaxws.EndpointImpl;
import org.apache.cxf.ext.logging.LoggingFeature;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import ru.mos.emias.pushaccepterproduct.pushaccepterservice.v1.GetService;

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
        endpoint.publish(); // http://localhost:8080/nsi/pushaccepterService?wsdl

        //endpoint.getInInterceptors().add(soapVersionInterceptor);
//        endpoint.getInInterceptors().add(credentialsValidator());
//        interceptorService.setupInterceptors(endpoint);

        return endpoint;
    }
}
