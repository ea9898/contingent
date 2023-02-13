package moscow.ptnl.contingent.attachment.configuration;

import io.micrometer.core.instrument.Clock;
import io.micrometer.core.instrument.MeterRegistry;
import io.micrometer.prometheus.PrometheusConfig;
import io.micrometer.prometheus.PrometheusMeterRegistry;
import io.prometheus.client.CollectorRegistry;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.web.servlet.ServletRegistrationBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import jakarta.servlet.Servlet;
import moscow.ptnl.contingent.attachment.ws.v1.AttachmentServiceImpl;
import moscow.ptnl.metrics.bind.JvmBufferPoolMetrics;
import moscow.ptnl.metrics.bind.ProcessMemoryMetrics;
import moscow.ptnl.metrics.bind.ServiceMetrics;
import moscow.ptnl.metrics.bind.SystemMetrics;
import moscow.ptnl.metrics.servlet.MetricsServlet;
import ru.mos.emias.metrics.SDNamingConvention;
import ru.mos.emias.metrics.jvm.SDClassLoaderMetrics;
import ru.mos.emias.metrics.jvm.SDJvmGcMetrics;
import ru.mos.emias.metrics.jvm.SDJvmMemoryMetrics;
import ru.mos.emias.metrics.jvm.SDJvmThreadMetrics;

/**
 *
 * @author m.kachalov
 */
@Configuration
public class MetricsConfiguration {
    
    private final static Logger LOG = LoggerFactory.getLogger(MetricsConfiguration.class);
        
    @Bean
    public PrometheusMeterRegistry meterRegistry() {
        PrometheusMeterRegistry meterRegistry =
                new PrometheusMeterRegistry(PrometheusConfig.DEFAULT, CollectorRegistry.defaultRegistry, Clock.SYSTEM);
        meterRegistry.config().namingConvention(new SDNamingConvention());
        
        new SDJvmThreadMetrics().bindTo(meterRegistry);
        
        new SDJvmGcMetrics().bindTo(meterRegistry);
        
        new SDClassLoaderMetrics().bindTo(meterRegistry);
        
        new JvmBufferPoolMetrics().bindTo(meterRegistry);
        
        new SDJvmMemoryMetrics().bindTo(meterRegistry);
        
        new ProcessMemoryMetrics().bindTo(meterRegistry);
        
        new SystemMetrics().bindTo(meterRegistry);

        return meterRegistry;
    }

    @Bean
    public ServletRegistrationBean<Servlet> metricsServletRegistrationBean(PrometheusMeterRegistry service) {
        MetricsServlet metricsServlet = new MetricsServlet(service);
        ServletRegistrationBean<Servlet> regBeen = new ServletRegistrationBean<>(metricsServlet,
                "/metrics");
        regBeen.setLoadOnStartup(4);

        return regBeen;
    }

    @Bean
    public ServiceMetrics serviceMetrics(MeterRegistry registry) {
        ServiceMetrics metrics = new ServiceMetrics(AttachmentServiceImpl.class);
        metrics.bindTo(registry);
        return metrics;
    }
}
