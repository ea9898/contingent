package moscow.ptnl.contingent.nsi.configuration;

import org.apache.cxf.transport.servlet.CXFServlet;
import org.springframework.web.WebApplicationInitializer;
import org.springframework.web.context.ContextLoaderListener;
import org.springframework.web.context.support.AnnotationConfigWebApplicationContext;

import javax.servlet.ServletException;
import javax.servlet.ServletRegistration;
import java.util.concurrent.TimeUnit;

public class ContingentNsiApplicationInitializer implements WebApplicationInitializer {

    @Override
    public void onStartup(javax.servlet.ServletContext servletContext) throws ServletException {
        AnnotationConfigWebApplicationContext dispatcherContext = new AnnotationConfigWebApplicationContext();
        servletContext.addListener(new ContextLoaderListener(dispatcherContext));
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
    }
}
