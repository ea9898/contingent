package moscow.ptnl.contingent.attachment.configuration;

import io.micrometer.core.instrument.Clock;
import io.micrometer.core.instrument.MeterRegistry;
import io.micrometer.core.instrument.binder.jvm.DiskSpaceMetrics;
import io.micrometer.prometheus.PrometheusConfig;
import io.micrometer.prometheus.PrometheusMeterRegistry;
import io.prometheus.client.CollectorRegistry;
import moscow.ptnl.contingent.attachment.service.AttachmentServiceInternal;
import moscow.ptnl.contingent.attachment.service.AttachmentServiceInternalImpl;
import moscow.ptnl.metrics.bind.JvmBufferPoolMetrics;
import moscow.ptnl.metrics.bind.ProcessMemoryMetrics;
import moscow.ptnl.metrics.bind.ServiceMetrics;
import moscow.ptnl.metrics.bind.SystemMetrics;
import moscow.ptnl.metrics.bind.WildFlyDataSourceMetrics;
import moscow.ptnl.metrics.bind.WildFlyUndertowMetrics;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import ru.mos.emias.metrics.SDNamingConvention;
import ru.mos.emias.metrics.jvm.SDClassLoaderMetrics;
import ru.mos.emias.metrics.jvm.SDJvmGcMetrics;
import ru.mos.emias.metrics.jvm.SDJvmMemoryMetrics;
import ru.mos.emias.metrics.jvm.SDJvmThreadMetrics;

import javax.management.MBeanServer;
import javax.sql.DataSource;
import java.io.File;
import java.lang.management.ManagementFactory;
import java.util.stream.Collectors;

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
        
        new DiskSpaceMetrics(new File("/")).bindTo(meterRegistry);
                       
        /*
        new ClassLoaderMetrics().bindTo(meterRegistry);
                
        new JvmMemoryMetrics().bindTo(meterRegistry);
                
        new ProcessorMetrics().bindTo(meterRegistry);
        
        new ProcessThreadMetrics().bindTo(meterRegistry);
        */
        
        return meterRegistry;
    }
    
    @Bean
    public WildFlyDataSourceMetrics dataSourceMetrics(MeterRegistry registry, MBeanServer mBeanServer, ObjectProvider<DataSource> dataSources) {
        WildFlyDataSourceMetrics metrics = new WildFlyDataSourceMetrics(mBeanServer, dataSources.stream().collect(Collectors.toList()).toArray(new DataSource[0]));
        metrics.bindTo(registry);
        return metrics;
    }
    
    @Bean
    public WildFlyUndertowMetrics undertowMetrics(MeterRegistry registry, MBeanServer mBeanServer) {
        WildFlyUndertowMetrics metrics = new WildFlyUndertowMetrics(mBeanServer);
        metrics.bindTo(registry);
        return metrics;
    }
        
    @Bean
    public ServiceMetrics serviceMetrics(MeterRegistry registry) {
        ServiceMetrics metrics = new ServiceMetrics(AttachmentServiceInternalImpl.class);
        metrics.bindTo(registry);
        return metrics;
    }
    
    @Bean
    public MBeanServer mBeanServer() {
        return ManagementFactory.getPlatformMBeanServer();
    }    
        
}
