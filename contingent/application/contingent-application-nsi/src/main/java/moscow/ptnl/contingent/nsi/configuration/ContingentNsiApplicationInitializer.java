package moscow.ptnl.contingent.nsi.configuration;

import moscow.ptnl.contingent.infrastructure.health_check.HealthCheckService;
import moscow.ptnl.contingent.infrastructure.health_check.HealthCheckServlet;

import org.apache.cxf.transport.servlet.CXFServlet;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.WebApplicationInitializer;
import org.springframework.web.context.ContextLoaderListener;
import org.springframework.web.context.support.AnnotationConfigWebApplicationContext;

import javax.servlet.ServletContextEvent;
import javax.servlet.ServletException;
import javax.servlet.ServletRegistration;
import java.lang.invoke.MethodHandles;

public class ContingentNsiApplicationInitializer implements WebApplicationInitializer {

    private final static Logger LOG = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass().getName());

    @Override
    public void onStartup(javax.servlet.ServletContext servletContext) throws ServletException {
        AnnotationConfigWebApplicationContext dispatcherContext = new AnnotationConfigWebApplicationContext();

        HealthCheckServlet healthCheckServlet = new HealthCheckServlet();

        servletContext.addListener(new ContextLoaderListener(dispatcherContext){
            @Override
            public void contextInitialized(ServletContextEvent event) {
                super.contextInitialized(event);
                try {
                    //Регистрация метрик в сервлете
                    healthCheckServlet.setService(dispatcherContext.getBean(HealthCheckService.class));
                } catch (Exception e) {
                    LOG.error("ошибка инициализации", e);
                    throw new IllegalStateException(e);
                }
            }
        });
        dispatcherContext.register(new Class[]{
                WebServiceConfiguration.class
        });
        ServletRegistration.Dynamic cXFServlet = servletContext.addServlet("CXFServlet", new CXFServlet());
        cXFServlet.setLoadOnStartup(1);
        //cXFServlet.addMapping("/area/*");
        cXFServlet.addMapping("/*");

        //Регистрация сервлетов метрик
//        ServletRegistration.Dynamic metrics = container.addServlet("metrics", new MetricsServlet(new MetricRegistry() {{
//            registerAll(new GarbageCollectorMetricSet());
//            registerAll(new CachedThreadStatesGaugeSet(10, TimeUnit.SECONDS));
//            registerAll(new MemoryUsageGaugeSet());
//        }}));
//        metrics.setLoadOnStartup(2);
//        metrics.addMapping("/metrics");
//        ServletRegistration.Dynamic health = container.addServlet("health", new HealthCheckServlet(new HealthCheckRegistry() {{
//            register("jvm.deadlock", new ThreadDeadlockHealthCheck());
//        }}));
//        health.setLoadOnStartup(3);
//        health.addMapping("/health");

        ServletRegistration.Dynamic healthServlet = servletContext.addServlet("healthCheck", healthCheckServlet);
        healthServlet.setLoadOnStartup(3);
        healthServlet.addMapping("/health");
    }
}
