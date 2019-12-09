package moscow.ptnl.contingent.area.configuration;

import io.github.mweirauch.micrometer.jvm.extras.ProcessThreadMetrics;
import io.micrometer.core.instrument.Clock;
import io.micrometer.core.instrument.MeterRegistry;
import io.micrometer.core.instrument.binder.jvm.ClassLoaderMetrics;
import io.micrometer.core.instrument.binder.jvm.DiskSpaceMetrics;
import io.micrometer.core.instrument.binder.jvm.JvmGcMetrics;
import io.micrometer.core.instrument.binder.jvm.JvmMemoryMetrics;
import io.micrometer.core.instrument.binder.jvm.JvmThreadMetrics;
import io.micrometer.core.instrument.binder.system.ProcessorMetrics;
import io.micrometer.prometheus.PrometheusConfig;
import io.micrometer.prometheus.PrometheusMeterRegistry;
import io.prometheus.client.CollectorRegistry;
import java.io.File;
import java.lang.management.ManagementFactory;
import javax.management.MBeanServer;
import javax.sql.DataSource;
import moscow.ptnl.contingent.area.ws.v1.AreaServiceImpl;
import moscow.ptnl.metrics.bind.JMXMetrics;
import moscow.ptnl.metrics.bind.ProcessMemoryMetrics;
import moscow.ptnl.metrics.bind.ServiceMetrics;
import moscow.ptnl.metrics.bind.SystemMetrics;
import moscow.ptnl.metrics.bind.WildFlyDataSourceMetrics;
import moscow.ptnl.metrics.bind.WildFlyUndertowMetrics;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

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
        //List<Tag> tags = new ArrayList<>();
        //tags.add(Tag.of("node", CommonUtils.getHostName()));
        //meterRegistry.config().commonTags(tags);
        
        
        new ClassLoaderMetrics().bindTo(meterRegistry);
                
        new JvmMemoryMetrics().bindTo(meterRegistry);
                
        new JvmThreadMetrics().bindTo(meterRegistry);
                
        new ProcessorMetrics().bindTo(meterRegistry);
                        
        new JvmGcMetrics().bindTo(meterRegistry);
                
        new DiskSpaceMetrics(new File("/")).bindTo(meterRegistry);
                
        new ProcessMemoryMetrics().bindTo(meterRegistry);
        
        new ProcessThreadMetrics().bindTo(meterRegistry);
        
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
    public SystemMetrics systemMetrics(MeterRegistry registry) {
        SystemMetrics metrics = new SystemMetrics();
        metrics.bindTo(registry);
        return metrics;
    }
        
    @Bean
    public ServiceMetrics serviceMetrics(MeterRegistry registry) {
        ServiceMetrics metrics = new ServiceMetrics(AreaServiceImpl.class);
        metrics.bindTo(registry);
        return metrics;
    }
    
    
    @Bean
    public MBeanServer mBeanServer() {
        return ManagementFactory.getPlatformMBeanServer();
    }
        
}
