/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
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
import moscow.ptnl.metrics.bind.DataSourceMetrics;
import moscow.ptnl.metrics.bind.JMXMetrics;
import moscow.ptnl.metrics.bind.WildFlyUndertowMetrics;
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
        new ClassLoaderMetrics().bindTo(meterRegistry);
        new JvmMemoryMetrics().bindTo(meterRegistry);
        new JvmGcMetrics().bindTo(meterRegistry);
        new JvmThreadMetrics().bindTo(meterRegistry);
        new ProcessorMetrics().bindTo(meterRegistry);
        new FileDescriptorMetrics().bindTo(meterRegistry);
        
        return meterRegistry;
    }
    
    @Bean
    public DataSourceMetrics dataSourceMetrics(MeterRegistry registry, DataSource dataSource) {
        DataSourceMetrics metrics = new DataSourceMetrics(dataSource);
        metrics.bindTo(registry);
        return metrics;
    }
    
    
    @Bean
    public JMXMetrics wildFlyMetrics(MeterRegistry registry, MBeanServer mBeanServer) {
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
