package moscow.ptnl.contingent.area.configuration;

import java.util.Properties;
import java.util.ResourceBundle;
import javax.naming.NamingException;
import javax.persistence.EntityManagerFactory;
import javax.sql.DataSource;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.PropertySource;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;
import org.springframework.jndi.JndiTemplate;
import org.springframework.orm.jpa.JpaTransactionManager;
import org.springframework.orm.jpa.LocalContainerEntityManagerFactoryBean;
import org.springframework.orm.jpa.vendor.HibernateJpaVendorAdapter;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.annotation.EnableTransactionManagement;

/**
 *
 * @author m.kachalov
 */
@Configuration
@EnableTransactionManagement
@EnableJpaRepositories(
        basePackages = "moscow.ptnl.contingent.area.repository",
        entityManagerFactoryRef = "contingentManagerFactory", 
        transactionManagerRef = "contingentTransactionManager"
)
@PropertySource("classpath:/application.properties")
public class PersistenceConfiguration {
    
    private static final Logger logger = LoggerFactory.getLogger(PersistenceConfiguration.class);
    
    public static final String PU_CONTINGENT_NAME = "contingent";
    
    @Value("${datasource.contingent.jndi-name}")
    private String contingentDataSourceJNDIName;
    
    @Bean(name = "contingentDataSource")
    public DataSource dataSource() {
        DataSource dataSource = null;
        try {
            JndiTemplate jndi = new JndiTemplate();
            dataSource = jndi.lookup(contingentDataSourceJNDIName, DataSource.class);
            logger.info("DataSource init " + PersistenceConfiguration.class);
            System.out.println("DataSource created");
        } catch (NamingException e) {
            logger.error("Error DataSource init " + PersistenceConfiguration.class);
            System.err.println(e);
        }
        return dataSource;
    }
    
    @Bean(name = "contingentManagerFactory") 
    public EntityManagerFactory entityManagerFactory(@Qualifier("contingentDataSource") DataSource dataSource) {
        ResourceBundle bundle = ResourceBundle.getBundle("application");
        HibernateJpaVendorAdapter vendorAdapter = new HibernateJpaVendorAdapter();
        vendorAdapter.setShowSql(true);
        vendorAdapter.setGenerateDdl(false);
        LocalContainerEntityManagerFactoryBean factory = new LocalContainerEntityManagerFactoryBean();
        factory.setJpaVendorAdapter(vendorAdapter);
        Properties jpaProperties = new Properties();        
        jpaProperties.setProperty("hibernate.jdbc.fetch_size", bundle.getString("org.hibernate.fetchSize"));
        jpaProperties.setProperty("javax.persistence.sharedCache.mode", bundle.getString("spring.jpa.properties.javax.persistence.sharedCache.mode"));
        jpaProperties.setProperty("hibernate.cache.use_second_level_cache", bundle.getString("spring.jpa.properties.hibernate.cache.use_second_level_cache"));
        jpaProperties.setProperty("hibernate.cache.use_query_cache", bundle.getString("spring.jpa.properties.hibernate.cache.use_query_cache"));
        jpaProperties.setProperty("hibernate.cache.region.factory_class", bundle.getString("spring.jpa.properties.hibernate.cache.region.factory_class"));        
        jpaProperties.setProperty("hibernate.cache.infinispan.statistics", bundle.getString("spring.jpa.properties.hibernate.cache.infinispan.statistics"));
        jpaProperties.setProperty("hibernate.generate_statistics", bundle.getString("spring.jpa.properties.hibernate.generate_statistics"));
        factory.setJpaProperties(jpaProperties);
        factory.setPackagesToScan("moscow.ptnl");
        factory.setDataSource(dataSource);
        factory.setPersistenceUnitName(PU_CONTINGENT_NAME);
        factory.afterPropertiesSet();
        return factory.getObject();
    }
    
    @Bean (name = "contingentTransactionManager")
    public PlatformTransactionManager transactionManager(@Qualifier("contingentManagerFactory") EntityManagerFactory entityManagerFactory) {
        JpaTransactionManager txManager = new JpaTransactionManager();
        txManager.setEntityManagerFactory(entityManagerFactory);
        return txManager;
    }
    
}
