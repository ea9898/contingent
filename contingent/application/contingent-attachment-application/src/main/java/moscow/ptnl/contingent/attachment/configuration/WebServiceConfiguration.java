package moscow.ptnl.contingent.attachment.configuration;

import moscow.ptnl.contingent.area.configuration.EventChannelsConfiguration;
import moscow.ptnl.contingent.attachment.ws.security.UserContextHolder;
import moscow.ptnl.contingent.attachment.ws.security.UserContextInterceptor;
import moscow.ptnl.metrics.MetricsInterceptorService;
import moscow.ptnl.soap.log.SoapLogInterceptorService;
import org.apache.cxf.Bus;
import org.apache.cxf.bus.spring.SpringBus;
import org.apache.cxf.ext.logging.LoggingFeature;
import org.apache.cxf.jaxws.EndpointImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.messaging.MessageChannel;
import org.springframework.scheduling.annotation.EnableAsync;

import javax.xml.namespace.QName;
import javax.xml.ws.Endpoint;
import java.util.ArrayList;
import java.util.Arrays;

@Configuration
@ComponentScan(basePackages = "moscow.ptnl")
@EnableAsync
public class WebServiceConfiguration {
    
    @Autowired
    private MetricsInterceptorService metricsInterceptorService;

    @Autowired
    private SoapLogInterceptorService soapLogInterceptorService;

    @Autowired
    private SoapVersionInterceptor soapVersionInterceptor;

    @Autowired
    @Qualifier(EventChannelsConfiguration.SOAP_LOG_EVENT_CHANNEL_NAME)
    private MessageChannel soapLogChannel;

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
    public Endpoint AttachmentServiceV1(@Qualifier(moscow.ptnl.contingent.attachment.ws.v1.AttachmentServiceImpl.SERVICE_NAME) ru.mos.emias.contingent2.attachment.v1.AttachmentPT attachmentPT, SpringBus cxfBus) {
        EndpointImpl endpoint = new EndpointImpl(cxfBus, attachmentPT);
        initAttachmentService(endpoint, "v1");
        return endpoint;
    }

    private void initAttachmentService(EndpointImpl endpoint, String version) {
        String pathPart = (version != null && !version.isEmpty()) ? version + "/" : "";
        endpoint.setServiceName(new QName("http://emias.mos.ru/contingent2/attachment/" + pathPart, "AttachmentService"));
        String wsdlLocation = "classpath:META-INF/wsdl/contingent2/" + (pathPart.isEmpty() ? "v1/" : pathPart) + "emias.contingent2.attachment." + (version == null ? "v1" : version) + ".wsdl";
        endpoint.setWsdlLocation(wsdlLocation);
        endpoint.setAddress("/" + pathPart + "attachmentService");
        endpoint.publish();

    	endpoint.getInInterceptors().add(soapVersionInterceptor);
        endpoint.getInInterceptors().add(credentialsValidator());
        metricsInterceptorService.setupInterceptors(endpoint);
        soapLogInterceptorService.setupInterceptors(endpoint, soapLogChannel, UserContextHolder::getRequestId);
    }
    
}
