package moscow.ptnl.metrics.bind;

import io.micrometer.core.instrument.Gauge;
import io.micrometer.core.instrument.MeterRegistry;
import io.micrometer.core.instrument.Tag;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import javax.management.MBeanAttributeInfo;
import javax.management.MBeanServer;
import javax.management.ObjectName;

/**
 * Сбор статистики по http(s) коннекторам WildFly.
 * Предварительно сбор статистики должен быть включен в настройках WildFly, 
 * например командой: /subsystem=undertow:write-attribute(name=statistics-enabled, value=true)
 * через jboss-cli.
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
        
        tags.add(Tag.of("server", beanName.getKeyProperty("server")));
        
        String listener = null;
        String name = null;
        if (beanName.getKeyProperty("http-listener") != null) {
            listener = "http";
            name = beanName.getKeyProperty("http-listener");
        } else if (beanName.getKeyProperty("https-listener") != null) {
            listener = "https";
            name = beanName.getKeyProperty("https-listener");
        }
        if (listener != null && name != null){
            tags.add(Tag.of("listener", listener));
            tags.add(Tag.of("name", name));
        }
        
        return tags;
    }

    @Override
    protected String[] getPropertiesNames() {
        return PROPERTIES;
    }
    
    
}
