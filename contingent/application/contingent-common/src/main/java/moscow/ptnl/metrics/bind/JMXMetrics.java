package moscow.ptnl.metrics.bind;

import io.micrometer.core.instrument.binder.MeterBinder;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import javax.management.MBeanAttributeInfo;
import javax.management.MBeanInfo;
import javax.management.MBeanServer;
import javax.management.MalformedObjectNameException;
import javax.management.ObjectName;

/**
 * https://github.com/prometheus/jmx_exporter/blob/master/example_configs/wildfly-10.yaml
 * Паттерн для всех MBean: "*.*:*"
 * @author m.kachalov
 */
public abstract class JMXMetrics implements MeterBinder {

    private final MBeanServer mBeanServer;
    
    public JMXMetrics(MBeanServer mBeanServer){
        this.mBeanServer = mBeanServer;
    }
    
    protected Set<ObjectName> getBeansByPattern(String pattern) throws MalformedObjectNameException {
        return getMBeanServer().queryNames(new ObjectName(pattern), null)
                .stream()
                .collect(Collectors.toSet());
    }
        
    protected Map<String, MBeanAttributeInfo> getBeanAttributes(ObjectName beanObjectName) throws Exception {
        MBeanInfo beanInfo = getMBeanServer().getMBeanInfo(beanObjectName);
        Map<String, MBeanAttributeInfo> attributes = new HashMap<>();
        for(MBeanAttributeInfo a : beanInfo.getAttributes()) {
            attributes.put(a.getName(), a);
        }
        return attributes; 
    }
    
    protected String getAttributeValue(ObjectName beanObjectName, String attributeName) throws Exception {
        return String.valueOf(getMBeanServer().getAttribute(beanObjectName, attributeName));
    }

    /**
     * @return the MBeanServer
     */
    public MBeanServer getMBeanServer() {
        return mBeanServer;
    }
    
}
