package moscow.ptnl.contingent.area.configuration;

import java.util.ArrayList;
import java.util.Arrays;
import javax.xml.namespace.QName;
import javax.xml.ws.Endpoint;
import moscow.ptnl.metrics.MetricsInterceptorService;
import moscow.ptnl.ws.security.UserContextInterceptor;
import org.apache.cxf.Bus;
import org.apache.cxf.bus.spring.SpringBus;
import org.apache.cxf.ext.logging.LoggingFeature;
import org.apache.cxf.jaxws.EndpointImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

/**
 *
 * @author m.kachalov
 */
@Configuration
@ComponentScan("moscow.ptnl")
public class WebServiceConfiguration {
    
    @Autowired
    private MetricsInterceptorService interceptorService;

    @Autowired
    private SoapVersionInterceptor soapVersionInterceptor;
    

    @Bean(name = Bus.DEFAULT_BUS_ID)
    SpringBus springBus(LoggingFeature loggingFeature) {
        SpringBus bus = new SpringBus();
        bus.setFeatures(new ArrayList<>(Arrays.asList(loggingFeature)));
        return bus;
    }
    
    @Bean
    LoggingFeature loggingFeature() {
        LoggingFeature loggingFeature = new LoggingFeature();          
        loggingFeature.setLimit(-1); //The default is 48 * 1024 https://redmine.ptnl.moscow/issues/30432
        return loggingFeature;
    }    
    
    @Bean
    UserContextInterceptor credentialsValidator() { 
        return new UserContextInterceptor(); 
    }
    
    @Bean 
    public Endpoint AreaServiceV1(@Qualifier(moscow.ptnl.contingent.area.ws.v1.AreaServiceImpl.SERVICE_NAME) ru.gov.emias2.contingent.v1._public.area.AreaPT areaService, SpringBus cxfBus) {
        EndpointImpl endpoint = new EndpointImpl(cxfBus, areaService);
        initAreaService(endpoint, "v1");
        return endpoint;
    }
    
    @Bean 
    public Endpoint AreaServiceV5(@Qualifier(moscow.ptnl.contingent.area.ws.v5.AreaServiceImpl.SERVICE_NAME) ru.gov.emias2.contingent.v5._public.area.AreaPT areaService, SpringBus cxfBus) {
        EndpointImpl endpoint = new EndpointImpl(cxfBus, areaService);
        initAreaService(endpoint, "v5");
        return endpoint;
    }
    
    //http://localhost:8080/contingent/area/v5/AreaService?wsdl
    private void initAreaService(EndpointImpl endpoint, String version) {
        String pathPart = (version != null && !version.isEmpty()) ? version + "/" : "";
        endpoint.setServiceName(new QName("http://emias2.gov.ru/contingent/" + pathPart + "public/area/", "AreaService"));
        endpoint.setWsdlLocation("classpath:META-INF/wsdl/contingent/" + (pathPart.isEmpty() ? "v1/" : pathPart) + "public-area-soap.wsdl");
        endpoint.setAddress("/" + pathPart + "AreaService");
        endpoint.publish();

    	endpoint.getInInterceptors().add(soapVersionInterceptor);
        endpoint.getInInterceptors().add(credentialsValidator());
        interceptorService.setupInterceptors(endpoint);
    }
    
}
