package moscow.ptnl.contingent.area.configuration;

import java.util.ArrayList;
import java.util.Arrays;
import javax.xml.namespace.QName;
import javax.xml.ws.Endpoint;

import moscow.ptnl.contingent.area.ws.sysop.SysopWebService;
import moscow.ptnl.contingent.area.ws.v1.AreaCompositeServiceImpl;
import moscow.ptnl.metrics.MetricsInterceptorService;
import moscow.ptnl.soap.log.SoapLogInterceptorService;
import moscow.ptnl.ws.security.UserContextHolder;
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
import org.springframework.messaging.MessageChannel;
import ru.mos.emias.contingent2.sysop.v1.SysopPT;

/**
 * Конфигурационный файл для создания Aoache CXF SOAP-сервисов.
 * 
 * Для включения логирования SOAP-запросов в настройках сервера приложений 
 * должно быть установлено свойство org.apache.cxf.logging.enabled=true.
 * 
 * @author m.kachalov
 */
@Configuration
@ComponentScan(basePackages = "moscow.ptnl")
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
    public Endpoint AreaServiceV1(@Qualifier(moscow.ptnl.contingent.area.ws.v1.AreaServiceImpl.SERVICE_NAME) ru.mos.emias.contingent2.area.AreaPT areaService, SpringBus cxfBus) {
        EndpointImpl endpoint = new EndpointImpl(cxfBus, areaService);
        initAreaService(endpoint, "v1");
        return endpoint;
    }

    @Bean
    public Endpoint AreaCompositServiceV1(@Qualifier(AreaCompositeServiceImpl.SERVICE_NAME) ru.mos.emias.contingent2.area.composit.AreaCompositePT areaCompositService, SpringBus cxfBus) {
        EndpointImpl endpoint = new EndpointImpl(cxfBus, areaCompositService);
        endpoint.setServiceName(new QName("http://emias.mos.ru/contingent2/area/composite/v1/", "AreaCompositeService"));
        endpoint.setWsdlLocation("classpath:META-INF/wsdl/composite/emias.contingent2.composite.v1.wsdl");
        endpoint.setAddress("/composite/v1/AreaCompositeService");
        endpoint.publish();
        endpoint.getInInterceptors().add(soapVersionInterceptor);
        endpoint.getInInterceptors().add(credentialsValidator());
        metricsInterceptorService.setupInterceptors(endpoint);
        soapLogInterceptorService.setupInterceptors(endpoint, soapLogChannel, UserContextHolder::getRequestId);
        return endpoint;
    }

    @Bean
    public Endpoint SysopService(@Qualifier(SysopWebService.SERVICE_NAME) SysopPT sysopService, SpringBus cxfBus) {
        EndpointImpl endpoint = new EndpointImpl(cxfBus, sysopService);
        endpoint.setServiceName(new QName("http://emias.mos.ru/contingent2/sysop/v1/", "SysopService"));
        endpoint.setWsdlLocation("classpath:META-INF/wsdl/sysop/emias.contingent2.sysop.v1.wsdl");
        endpoint.setAddress("v1/SysopService");
        endpoint.publish();
        endpoint.getInInterceptors().add(soapVersionInterceptor);
        endpoint.getInInterceptors().add(credentialsValidator());
        return endpoint;
    }

//    @Bean
//    public Endpoint AreaServiceV5(@Qualifier(moscow.ptnl.contingent.area.ws.v2.AreaServiceImpl.SERVICE_NAME) ru.gov.emias2.contingent.v2._public.area.AreaPT areaService, SpringBus cxfBus) {
//        EndpointImpl endpoint = new EndpointImpl(cxfBus, areaService);
//        initAreaService(endpoint, "v2");
//        return endpoint;
//    }
    
    private void initAreaService(EndpointImpl endpoint, String version) {
        String pathPart = (version != null && !version.isEmpty()) ? version + "/" : "";
        endpoint.setServiceName(new QName("http://emias.mos.ru/contingent2/area/" + pathPart, "AreaService"));
        String wsdlLocation = "classpath:META-INF/wsdl/contingent2/" + (pathPart.isEmpty() ? "v1/" : pathPart) + "emias.contingent2." + (version == null ? "v1" : version) + ".wsdl";
        endpoint.setWsdlLocation(wsdlLocation);
        endpoint.setAddress("/" + pathPart + "AreaService");
        endpoint.publish();

    	endpoint.getInInterceptors().add(soapVersionInterceptor);
        endpoint.getInInterceptors().add(credentialsValidator());
        metricsInterceptorService.setupInterceptors(endpoint);
        soapLogInterceptorService.setupInterceptors(endpoint, soapLogChannel, UserContextHolder::getRequestId);
    }
    
}
