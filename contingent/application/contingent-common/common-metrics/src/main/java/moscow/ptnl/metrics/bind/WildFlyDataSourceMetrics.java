package moscow.ptnl.metrics.bind;

import io.micrometer.core.instrument.Gauge;
import io.micrometer.core.instrument.MeterRegistry;
import io.micrometer.core.instrument.Tag;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import javax.management.MBeanAttributeInfo;
import javax.management.MBeanServer;
import javax.management.ObjectName;
import javax.sql.DataSource;
import static moscow.ptnl.metrics.bind.JMXMetrics.toSnakeCase;

/**
 * Сбор статистики и проверка статуса датасорсов в WildFly.
 * Предварительно сбор статистики должен быть включен в настройках WildFly, 
 * например командами: 
 * 
 * /subsystem=datasources/data-source=ИмяПула/statistics=pool:write-attribute(name=statistics-enabled,value=true)
 * /subsystem=datasources/data-source=ИмяПула/statistics=jdbc:write-attribute(name=statistics-enabled,value=true)
 * 
 * через jboss-cli.
 * 
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
        
     //TODO это бы хорошо вынести в настройки, так как для разных БД запрос проверки коннекта разный или брать из настроек пула
    private static final String DEFAULT_CHECK_VALID_SQL_QUERY = "SELECT 1;";
    private static final int QUERY_TIMEOUT = 1;
    private static final double UP = 1.0;
    private static final double DOWN = 0.0;
    
    private final Map<String, DataSource> dataSources; //ключ - имя датасорса из настроек WildFly, значение - соответствующий DataSource
    private final Map<String, String> CHECK_VALID_SQL_QUERY; //запросы для проверки статуса датасоурсов - живой/не живой
        
    /**
     * 
     * @param mBeanServer
     * @param dataSources DataSource-ы, статистика по которым интересна
     */
    public WildFlyDataSourceMetrics(MBeanServer mBeanServer, DataSource ... dataSources) {
        super(mBeanServer, PATTERNS);
        this.dataSources = getDataSourceStatMBeans(new HashSet<>(Arrays.asList(dataSources)));
        this.CHECK_VALID_SQL_QUERY = fillValidationQuery();
        /*
        getMBeans().stream().filter(b -> this.dataSources.keySet().contains(b.getKeyProperty("data-source"))).forEach(beanName -> {
            this.getAttributesInfo(beanName).keySet().forEach(k -> System.out.println("PROP: " + k));
        });
        */
    }

    @Override
    public void bindTo(MeterRegistry meterRegistry) {
        getMBeans().stream().filter(b -> dataSources.keySet().contains(b.getKeyProperty("data-source"))).forEach(beanName -> {
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
            
            Gauge.builder(
                    "datasource", 
                    this, 
                    value -> value.status(beanName.getKeyProperty("data-source")) ? UP : DOWN)
                .description("DataSource status")
                .tags(tags(beanName))
                .baseUnit("status")
                .register(meterRegistry);
        });
    }
    
    private Map<String, String> fillValidationQuery() {
        Map<String, String> querys = new HashMap<>();
        dataSources.keySet().forEach(ds_name -> {
            try {
                ObjectName dsBean = this.getMBeansByPattern("jboss.as:subsystem=datasources,data-source=" + ds_name).iterator().next();
                Object value = getAttributesValues(dsBean, new String[] {"checkValidConnectionSql"}).get("checkValidConnectionSql");
                if (value != null && !((String) value).isEmpty()) {
                    querys.put(ds_name, (String) value);
                } else {
                   querys.put(ds_name, DEFAULT_CHECK_VALID_SQL_QUERY);
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        });
        return querys;
    }
    
    /**
     * Устанавливает соответствие между именем датасорса в WildFly и объектом
     * типа DataSource.
     * Использует для связи MBean-ов их URL и URL из DataSource. 
     * Такой способ будет работать неверно, если есть несколько конектов с 
     * одинаковым "connectionUrl" и разным "userName".
     * 
     * @param dataSources
     * @return
     * @throws Exception 
     */
    private Map<String, DataSource> getDataSourceStatMBeans(Set<DataSource> dataSources) {
        
        Map<String, DataSource> ds_map = new HashMap<>() ;
        
        try {
            Map<String, DataSource> urls = new HashMap<>();
            for (DataSource ds : dataSources) {
                String url = ds.getConnection().getMetaData().getURL();
                urls.put(url, ds);
            }
        
            Set<ObjectName> dsBeans = this.getMBeansByPattern("jboss.as:subsystem=datasources,data-source=*"); //все датасоурсы       
            dsBeans.forEach(b -> {
                try {
                    Map<String, MBeanAttributeInfo> attributes = getMBeanAttributesInfo(b);
                    MBeanAttributeInfo urlAttr = attributes.get("connectionUrl"); //альтернатвно, можно искать по атрибуту "jndiName" или добавить в сравнение "userName"
                    Map<String, Object> values = getAttributesValues(b, new String[] {"connectionUrl"});
                    String url = processBeanValue(urlAttr, values.get("connectionUrl"));
                    if (url != null && urls.keySet().contains(url)) {
                        String dataSourceName = b.getKeyProperty("data-source");
                        ds_map.put(dataSourceName, urls.get(url));
                    } 
                } catch (Exception e) {
                    e.printStackTrace();
                }
            });
        } catch (Exception e) {
            e.printStackTrace();
        }
        
        return ds_map;
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
    
    /**
     * Проверяет статус доступности датасорса.
     * Проверка статуса выполяется отправкой реального запроса.
     * 
     * @param dataSourceName
     * @return 
     */
    private boolean status(String dataSourceName) {
        DataSource dataSource = this.dataSources.get(dataSourceName);
        try(Connection connection = dataSource.getConnection()) {
            PreparedStatement statement = connection.prepareStatement(CHECK_VALID_SQL_QUERY.get(dataSourceName));
            statement.setQueryTimeout(QUERY_TIMEOUT);
            statement.executeQuery();
            return true;
        } catch (SQLException ignored) {
            return false;
        }
    }
    
}
