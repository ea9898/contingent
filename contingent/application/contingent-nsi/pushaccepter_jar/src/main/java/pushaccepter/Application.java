package pushaccepter;

import org.apache.cxf.transport.servlet.CXFServlet;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.SingletonBeanRegistry;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.jdbc.DataSourceAutoConfiguration;
import org.springframework.boot.autoconfigure.orm.jpa.HibernateJpaAutoConfiguration;
import org.springframework.boot.web.servlet.ServletRegistrationBean;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.ImportResource;
import org.springframework.scheduling.TaskScheduler;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.concurrent.ConcurrentTaskScheduler;
import org.springframework.ws.soap.server.endpoint.SoapFaultDefinition;
import ru.mos.emias.pushaccepterproduct.pushaccepterservice.v1.types.ChangeElement;
import ru.mos.emias.pushaccepterproduct.pushaccepterservice.v1.types.ResponseElement;

import java.util.List;

@SpringBootApplication
@EnableScheduling
@ComponentScan(value = {"pushaccepter"})
@EnableAutoConfiguration(exclude = {DataSourceAutoConfiguration.class, HibernateJpaAutoConfiguration.class})
@ImportResource({"classpath*:apache-cxf-services.xml"})
public class Application {
    @Autowired
    private ApplicationContext applicationContext;
    private PushAccepter pushAccepter;
    private List<Class> registerBeans;

    public Application() {
    }

    public Application(PushAccepter pushAccepter) {
        this.pushAccepter = pushAccepter;
    }

    public Application(PushAccepter pushAccepter, List<Class> registerBeans) {
        this.pushAccepter = pushAccepter;
        this.registerBeans = registerBeans;
    }

    public void run() {
        ConfigurableApplicationContext context = SpringApplication.run(Application.class);
        SingletonBeanRegistry beanRegistry = context.getBeanFactory();
        beanRegistry.registerSingleton("pushAccepter", pushAccepter);
    }


    public ResponseElement get(ChangeElement changeElement) {
        PushAccepter pushAccepter = applicationContext.getBean(PushAccepter.class);
        return pushAccepter.get(changeElement);
    }

    @Bean
    public TaskScheduler taskScheduler() {
        return new ConcurrentTaskScheduler();
    }

    @Bean(name = "soapFaultAnnotationExceptionResolver")
    public DefaultExceptionResolver exceptionResolver(ApplicationContext applicationContext) {
        DefaultExceptionResolver exceptionResolver = new DefaultExceptionResolver();

        SoapFaultDefinition soapFaultDefinition = new SoapFaultDefinition();
        soapFaultDefinition.setFaultCode(SoapFaultDefinition.SERVER);
        exceptionResolver.setDefaultFault(soapFaultDefinition);

        return exceptionResolver;
    }

    @Bean
    public ServletRegistrationBean dispatcherServlet() {
        return new ServletRegistrationBean(new CXFServlet(), "/services/*");
    }
}

