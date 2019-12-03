package moscow.ptnl.contingent.configuration;

import io.micrometer.core.instrument.Clock;
import io.micrometer.core.instrument.MeterRegistry;
import io.micrometer.core.instrument.binder.jvm.ClassLoaderMetrics;
import io.micrometer.core.instrument.binder.jvm.JvmGcMetrics;
import io.micrometer.core.instrument.binder.jvm.JvmMemoryMetrics;
import io.micrometer.core.instrument.binder.jvm.JvmThreadMetrics;
import io.micrometer.core.instrument.binder.system.FileDescriptorMetrics;
import io.micrometer.core.instrument.binder.system.ProcessorMetrics;
import io.micrometer.prometheus.PrometheusConfig;
import io.micrometer.prometheus.PrometheusMeterRegistry;
import io.prometheus.client.CollectorRegistry;
import java.lang.management.ManagementFactory;
import javax.management.MBeanServer;
import javax.sql.DataSource;
import moscow.ptnl.metrics.bind.JMXMetrics;
import moscow.ptnl.metrics.bind.WildFlyDataSourceMetrics;
import moscow.ptnl.metrics.bind.WildFlyUndertowMetrics;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 *
 * @author m.kachalov
 */
@Configuration
public class MetricsConfiguration {
        
    @Bean
    public PrometheusMeterRegistry meterRegistry() {
        PrometheusMeterRegistry meterRegistry = 
                new PrometheusMeterRegistry(PrometheusConfig.DEFAULT, CollectorRegistry.defaultRegistry, Clock.SYSTEM);
        //List<Tag> tags = new ArrayList<>();
        //tags.add(Tag.of("node", CommonUtils.getHostName()));
        //meterRegistry.config().commonTags(tags);
        new ClassLoaderMetrics().bindTo(meterRegistry);
        new JvmMemoryMetrics().bindTo(meterRegistry);
        new JvmGcMetrics().bindTo(meterRegistry);
        new JvmThreadMetrics().bindTo(meterRegistry);
        new ProcessorMetrics().bindTo(meterRegistry);
        new FileDescriptorMetrics().bindTo(meterRegistry);
        
        return meterRegistry;
    }
    
    
    @Bean
    public WildFlyDataSourceMetrics dataSourceMetrics(MeterRegistry registry, MBeanServer mBeanServer, @Qualifier("contingentDataSource") DataSource dataSource) {
        WildFlyDataSourceMetrics metrics = new WildFlyDataSourceMetrics(mBeanServer, dataSource);
        metrics.bindTo(registry);
        return metrics;
    }
        
    
    @Bean
    public JMXMetrics undertowMetrics(MeterRegistry registry, MBeanServer mBeanServer) {
        //undertow_request_count
        WildFlyUndertowMetrics metrics = new WildFlyUndertowMetrics(mBeanServer);
        metrics.bindTo(registry);
        return metrics;
    }
    
    
    @Bean
    public MBeanServer mBeanServer() {
        return ManagementFactory.getPlatformMBeanServer();
    }
        
}
