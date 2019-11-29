/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package moscow.ptnl.metrics.bind;

import io.micrometer.core.instrument.Gauge;
import io.micrometer.core.instrument.MeterRegistry;
import java.util.Map;
import javax.management.MBeanAttributeInfo;
import javax.management.MBeanServer;
import javax.management.ObjectName;

/**
 *
 * @author mkachalov
 */
public class WildFlyUndertowMetrics extends JMXMetrics {
    
    private static final String[] PATTERNS = new String[]{
        "jboss.as:subsystem=undertow,server=*,http-listener=*"
//        ,"jboss.as:subsystem=undertow,server=*,https-listener=*"
    };
    
    private static final String[] PROPERTIES = new String[] {
        "requestCount",
        "errorCount",
        "processingTime",
        "bytesSent",
        "bytesReceived"
        
    };
    
    public WildFlyUndertowMetrics(MBeanServer mBeanServer) {
        super(mBeanServer, PATTERNS);
    }

    @Override
    public void bindTo(MeterRegistry meterRegistry) {
        
        for (String property : PROPERTIES) {
            MBeanAttributeInfo info = getAttributeInfo(property);

            Gauge
                .builder("undertow_" + toSnakeCase(property), this, o -> Double.valueOf(processBeanValue(info, o.getValues().get(property))))
                .description(info.getDescription())
                //.tags(tags)
                .baseUnit("undertow")
                .register(meterRegistry);
        }
    } 
                
    private Map<String, Object> getValues() {
        try {
            ObjectName beanName = getMBeans().iterator().next();
            return getAttributesValues(beanName, PROPERTIES);
        } catch (Exception e) {
            e.printStackTrace();
            throw new RuntimeException(e);
        }        
    }
    
    private MBeanAttributeInfo getAttributeInfo(String name) {
        try {
            ObjectName beanName = getMBeans().iterator().next();
            return getAttributesInfo(beanName).get(name);
        } catch (Exception e) {
            e.printStackTrace();
            throw new RuntimeException(e);
        }
    }
    
    
}
