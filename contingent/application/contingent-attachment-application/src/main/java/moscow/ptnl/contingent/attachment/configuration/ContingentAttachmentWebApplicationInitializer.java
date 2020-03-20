package moscow.ptnl.contingent.attachment.configuration;

import io.micrometer.prometheus.PrometheusMeterRegistry;
import moscow.ptnl.metrics.servlet.MetricsServlet;
import org.apache.cxf.transport.servlet.CXFServlet;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.WebApplicationInitializer;
import org.springframework.web.context.ContextLoaderListener;
import org.springframework.web.context.support.AnnotationConfigWebApplicationContext;

import javax.servlet.ServletContext;
import javax.servlet.ServletContextEvent;
import javax.servlet.ServletException;
import javax.servlet.ServletRegistration;
import java.lang.invoke.MethodHandles;

public class ContingentAttachmentWebApplicationInitializer implements WebApplicationInitializer {
    
    private final static Logger LOG = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass().getName());
    
    @Override
    public void onStartup(ServletContext container) throws ServletException {
        
        AnnotationConfigWebApplicationContext dispatcherContext = new AnnotationConfigWebApplicationContext();
        MetricsServlet metricsServlet = new MetricsServlet();

        container.addListener(new ContextLoaderListener(dispatcherContext){
            @Override
            public void contextInitialized(ServletContextEvent event) {
                super.contextInitialized(event);
                try {
                    //Регистрация метрик в сервлете
                    metricsServlet.setMeterRegistry(dispatcherContext.getBean(PrometheusMeterRegistry.class));
                } catch (Exception e) {
                    LOG.error("ошибка инициализации", e);
                    throw new IllegalStateException(e);
                }
            }
        });

        dispatcherContext.register(new Class[]{
                WebServiceConfiguration.class
        });
        
        ServletRegistration.Dynamic cXFServlet = container.addServlet("CXFServlet", new CXFServlet());
        cXFServlet.setLoadOnStartup(1);
        cXFServlet.addMapping("/*");
        //Настройка редиректа для файла info.json
        cXFServlet.setInitParameter("redirects-list", "/info.json");
        cXFServlet.setInitParameter("redirect-attributes", "javax.servlet.include.request_uri");
        cXFServlet.setInitParameter("redirect-servlet-name", "default");

        ServletRegistration.Dynamic metrics = container.addServlet("metrics", metricsServlet);
        metrics.setLoadOnStartup(2);
        metrics.addMapping("/metrics");
    }
    
}
