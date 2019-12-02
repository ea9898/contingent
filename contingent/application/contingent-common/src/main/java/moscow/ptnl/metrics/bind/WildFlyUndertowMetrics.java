package moscow.ptnl.metrics.bind;

import io.micrometer.core.instrument.Gauge;
import io.micrometer.core.instrument.MeterRegistry;
import io.micrometer.core.instrument.Tag;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import javax.management.MBeanAttributeInfo;
import javax.management.MBeanServer;
import javax.management.ObjectName;

/**
 *
 * @author mkachalov
 */
public class WildFlyUndertowMetrics extends JMXMetrics {
    
    private static final String[] PATTERNS = new String[]{
        "jboss.as:subsystem=undertow,server=*,http-listener=*",
        "jboss.as:subsystem=undertow,server=*,https-listener=*"
    };
    
    private static final String[] PROPERTIES = new String[] {
        "requestCount",
        "errorCount",
        "processingTime",
        "bytesSent",
        "bytesReceived"
    };
    
    private static final String[] TAGS = new String[] {
        "http-listener",
        "https-listener",
        "server"
    };
    
    public WildFlyUndertowMetrics(MBeanServer mBeanServer) {
        super(mBeanServer, PATTERNS);        
    }

    @Override
    public void bindTo(MeterRegistry meterRegistry) {
        for (ObjectName beanName : getMBeans()) {
            for (String property : PROPERTIES) {
                Optional<MBeanAttributeInfo> info = getAttributeInfo(beanName, property);
                if (info.isPresent()) {
                    Gauge
                        .builder("undertow_" + toSnakeCase(property), this, o -> Double.valueOf(processBeanValue(info.get(), o.getValues(beanName).get(property))))
                        .description(info.get().getDescription())
                        .tags(tags(beanName))
                        .register(meterRegistry);
                }
            }
        }
    }
    
    private List<Tag> tags(ObjectName beanName) {
        List<Tag> tags = new ArrayList<>();
        for (String tagName : TAGS) {
            String value = beanName.getKeyProperty(tagName);
            if (value != null) {
                tags.add(Tag.of(tagName, value));
            }
        }
        return tags;
    }

    @Override
    protected String[] getPropertiesNames() {
        return PROPERTIES;
    }
    
    
}
