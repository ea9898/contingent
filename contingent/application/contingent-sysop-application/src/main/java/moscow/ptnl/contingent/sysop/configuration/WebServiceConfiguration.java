package moscow.ptnl.contingent.sysop.configuration;

import moscow.ptnl.contingent.sysop.ws.interceptor.SoapVersionInterceptor;
import moscow.ptnl.contingent.sysop.ws.SysopWebService;
import moscow.ptnl.contingent.sysop.ws.interceptor.UserContextInterceptor;
//import moscow.ptnl.metrics.MetricsInterceptorService;
//import moscow.ptnl.soap.log.SoapLogInterceptorService;
import org.apache.cxf.bus.spring.SpringBus;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import ru.mos.emias.contingent2.sysop.v1.SysopPT;
import org.apache.cxf.jaxws.EndpointImpl;

import javax.xml.namespace.QName;
import jakarta.xml.ws.Endpoint;

/**
 * Конфигурационный файл для создания Apache CXF SOAP-сервисов.
 * 
 * Для включения логирования SOAP-запросов в настройках сервера приложений 
 * должно быть установлено свойство org.apache.cxf.logging.enabled=true.
 * 
 * @author m.kachalov
 */
@Configuration
public class WebServiceConfiguration {
    
    //@Autowired
    //private MetricsInterceptorService metricsInterceptorService;

    //@Autowired
    //private SoapLogInterceptorService soapLogInterceptorService;

    @Autowired
    private SoapVersionInterceptor soapVersionInterceptor;
    
    @Bean
    UserContextInterceptor credentialsValidator() {
        return new UserContextInterceptor(); 
    }

    @Bean
    public Endpoint SysopService(@Qualifier(SysopWebService.SERVICE_NAME) SysopPT sysopService, SpringBus cxfBus) {
        EndpointImpl endpoint = new EndpointImpl(cxfBus, sysopService);
        endpoint.setServiceName(new QName("http://emias.mos.ru/contingent2/sysop/v1/", "SysopService"));
        endpoint.setWsdlLocation("classpath:META-INF/wsdl/sysop/v1/emias.contingent2.sysop.v1.wsdl");
        endpoint.setAddress("/v1/sysopService");
        endpoint.publish();
        endpoint.getInInterceptors().add(soapVersionInterceptor);
        endpoint.getInInterceptors().add(credentialsValidator());
        return endpoint;
    }

}
