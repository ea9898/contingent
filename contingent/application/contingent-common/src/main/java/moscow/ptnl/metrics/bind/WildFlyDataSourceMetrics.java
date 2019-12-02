/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package moscow.ptnl.metrics.bind;

import io.micrometer.core.instrument.Gauge;
import io.micrometer.core.instrument.MeterRegistry;
import io.micrometer.core.instrument.Tag;
import java.lang.reflect.Method;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import javax.management.MBeanAttributeInfo;
import javax.management.MBeanServer;
import javax.management.ObjectName;
import javax.sql.DataSource;
import static moscow.ptnl.metrics.bind.JMXMetrics.toSnakeCase;

/**
 * https://www.appdynamics.com/community/exchange/jboss-monitoring-extension/
 * @author m.kachalov
 */
public class WildFlyDataSourceMetrics extends JMXMetrics {
    
    private static final String[] PATTERNS = new String[]{
        "jboss.as:subsystem=datasources,data-source=*",
        "jboss.as:subsystem=datasources,data-source=*,statistics=jdbc",
        "jboss.as:subsystem=datasources,data-source=*,statistics=pool"
    };
    
    private static final String[] PROPERTIES = new String[] {
        //статистика по пулу
        "ActiveCount",
        "InUseCount",
        "TotalCreationTime",
        "WaitCount",
        "TimedOut",
        "MaxUsedCount",
        "AvailableCount",
        //статистика по JDBC
        "PreparedStatementCacheCurrentSize",
        "PreparedStatementCacheHitCount",
        "PreparedStatementCacheAccessCount",
        "PreparedStatementCacheMissCount"
    };
    
    private final Set<DataSource> dataSources;
    private final Set<String> dataSourcesNames;
        
    /**
     * 
     * @param mBeanServer
     * @param dataSources DataSource-ы, статистика по которым интересна
     */
    public WildFlyDataSourceMetrics(MBeanServer mBeanServer, DataSource ... dataSources) {
        super(mBeanServer, PATTERNS);
        this.dataSources = new HashSet<>(Arrays.asList(dataSources));
        this.dataSourcesNames = getDataSourceStatMBeans(this.dataSources);
    }

    @Override
    public void bindTo(MeterRegistry meterRegistry) {
        getMBeans().stream().filter(b -> dataSourcesNames.contains(b.getKeyProperty("data-source"))).forEach(beanName -> {
            for (String property : PROPERTIES) {
                Optional<MBeanAttributeInfo> info = getAttributeInfo(beanName, property);
                if (info.isPresent()) {
                    String areaName = beanName.getKeyProperty("statistics");
                    Gauge
                        .builder("datasource_" + areaName + "_" + toSnakeCase(property), this, o -> Double.valueOf(processBeanValue(info.get(), o.getValues(beanName).get(property))))
                        .description(info.get().getDescription())
                        .tags(tags(beanName))
                        .register(meterRegistry);
                }
            }
        });
    }
    
    /**
     * Метод использует для идентификации нужных бинов URL из DataSource. 
     * Такой способ будет раотать неверно, если есть несколько конектов с 
     * одинаковым "connectionUrl" и разным "userName".
     * @param dataSources
     * @return
     * @throws Exception 
     */
    private Set<String> getDataSourceStatMBeans(Set<DataSource> dataSources) {
        
        Set<String> mBeansPatterns = new HashSet<>() ;
        
        try {
            Set<String> urls = new HashSet<>();
            for (DataSource ds : dataSources) {
                String url = ds.getConnection().getMetaData().getURL();
                System.out.println("URL: " + url);
                urls.add(url);
            }
        
            Set<ObjectName> dsBeans = this.getMBeansByPattern("jboss.as:subsystem=datasources,data-source=*"); //все датасоурсы       
            dsBeans.forEach(b -> {
                try {
                    Map<String, MBeanAttributeInfo> attributes = getMBeanAttributesInfo(b);
                    MBeanAttributeInfo urlAttr = attributes.get("connectionUrl"); //альтернатвно, можно искать по атрибуту "jndiName" или добавить в сравнение "userName"
                    Map<String, Object> values = getAttributesValues(b, new String[] {"connectionUrl"});
                    String url = processBeanValue(urlAttr, values.get("connectionUrl"));
                    if (url != null && urls.contains(url)) {
                        String dataSourceName = b.getKeyProperty("data-source");
                        //mBeansPatterns.add("jboss.as:subsystem=datasources,data-source=" + dataSourceName + ",statistics=jdbc");
                        //mBeansPatterns.add("jboss.as:subsystem=datasources,data-source=" + dataSourceName + ",statistics=pool");
                        mBeansPatterns.add(dataSourceName);
                    } 
                } catch (Exception e) {
                    e.printStackTrace();
                }
            });
        } catch (Exception e) {
            e.printStackTrace();
        }
        
        return mBeansPatterns;
    }
    
    @Override
    protected String[] getPropertiesNames() {
        return PROPERTIES;
    }
    
    private List<Tag> tags(ObjectName beanName) {
        List<Tag> tags = new ArrayList<>();
        tags.add(Tag.of("name", beanName.getKeyProperty("data-source")));
        return tags;
    }
    
}
