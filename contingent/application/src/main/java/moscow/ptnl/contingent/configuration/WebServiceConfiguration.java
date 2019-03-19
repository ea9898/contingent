package moscow.ptnl.contingent.configuration;

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
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 *
 * @author m.kachalov
 */
@Configuration
public class WebServiceConfiguration {
    
    @Autowired
    private MetricsInterceptorService interceptorService;

    @Autowired
    private SoapVersionInterceptor soapVersionInterceptor;
    
    @Bean(name = Bus.DEFAULT_BUS_ID)
    SpringBus springBus() {
        SpringBus bus = new SpringBus();
        bus.setFeatures(new ArrayList<>(Arrays.asList(loggingFeature())));
        return bus;
    }
    
    @Bean
    LoggingFeature loggingFeature() {
        LoggingFeature loggingFeature = new LoggingFeature();          
        loggingFeature.setLimit(-1); //The default is 48 * 1024 https://redmine.ptnl.moscow/issues/30432
        return loggingFeature;
    }
    
    @Bean
    UserContextInterceptor schoolWsCredentialsValidator() { 
        return new UserContextInterceptor(); 
    }
    
    @Bean @Autowired
    public Endpoint AreaService(ru.gov.emias2.contingent.v5._public.area.AreaPT areaService) {
        EndpointImpl endpoint = new EndpointImpl(springBus(), areaService);
        initSchoolWS(endpoint, "");
        return endpoint;
    }
    
    private void initSchoolWS(EndpointImpl endpoint, String version) {
        String pathPart = (version != null && !version.isEmpty()) ? version + "/" : "";
        endpoint.setServiceName(new QName("http://ptnl.moscow/contingent/school/" + pathPart + "soap/", "School"));
        endpoint.setWsdlLocation("classpath:META-INF/wsdl/school/" + (pathPart.isEmpty() ? "v1/" : pathPart) + "school-soap.wsdl");
        endpoint.setAddress("/" + pathPart + "School");
        endpoint.publish();

    	endpoint.getInInterceptors().add(soapVersionInterceptor);
        endpoint.getInInterceptors().add(schoolWsCredentialsValidator());
        interceptorService.setupInterceptors(endpoint);
    }
    
}
