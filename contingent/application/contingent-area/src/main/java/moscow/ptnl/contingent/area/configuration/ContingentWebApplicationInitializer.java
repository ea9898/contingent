package moscow.ptnl.contingent.area.configuration;

import java.lang.invoke.MethodHandles;
import java.util.concurrent.TimeUnit;

import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.ServletRegistration;

import com.codahale.metrics.MetricRegistry;
import com.codahale.metrics.health.HealthCheckRegistry;
import com.codahale.metrics.jvm.CachedThreadStatesGaugeSet;
import com.codahale.metrics.jvm.MemoryUsageGaugeSet;
import com.codahale.metrics.servlets.HealthCheckServlet;
import com.codahale.metrics.servlets.MetricsServlet;
import com.codahale.metrics.jvm.GarbageCollectorMetricSet;
import com.codahale.metrics.health.jvm.ThreadDeadlockHealthCheck;
import org.apache.cxf.transport.servlet.CXFServlet;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.WebApplicationInitializer;
import org.springframework.web.context.ContextLoaderListener;
import org.springframework.web.context.support.AnnotationConfigWebApplicationContext;

public class ContingentWebApplicationInitializer implements WebApplicationInitializer {

    private final static Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass().getName());
    

    @Override
    public void onStartup(ServletContext container) throws ServletException {
        AnnotationConfigWebApplicationContext dispatcherContext = new AnnotationConfigWebApplicationContext();
        container.addListener(new ContextLoaderListener(dispatcherContext));
        dispatcherContext.register(new Class[]{
            WebServiceConfiguration.class
        });
        ServletRegistration.Dynamic cXFServlet = container.addServlet("CXFServlet", new CXFServlet());
        cXFServlet.setLoadOnStartup(1);
        //cXFServlet.addMapping("/area/*");
        cXFServlet.addMapping("/*");
        //Настройка редиректа для файла info.json
        cXFServlet.setInitParameter("redirects-list", "/info.json");
        cXFServlet.setInitParameter("redirect-attributes", "javax.servlet.include.request_uri");
        cXFServlet.setInitParameter("redirect-servlet-name", "default");

        //Регистрация сервлетов метрик
        ServletRegistration.Dynamic metrics = container.addServlet("metrics", new MetricsServlet(new MetricRegistry() {{
                registerAll(new GarbageCollectorMetricSet());
                registerAll(new CachedThreadStatesGaugeSet(10, TimeUnit.SECONDS));
                registerAll(new MemoryUsageGaugeSet());
        }}));
        metrics.setLoadOnStartup(2);
        metrics.addMapping("/metrics");
        ServletRegistration.Dynamic health = container.addServlet("health", new HealthCheckServlet(new HealthCheckRegistry() {{
                register("jvm.deadlock", new ThreadDeadlockHealthCheck());
        }}));
        health.setLoadOnStartup(3);
        health.addMapping("/health");
    }
}
