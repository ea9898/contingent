package moscow.ptnl.contingent.nsi.configuration;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;
import org.springframework.context.annotation.PropertySource;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;
import org.springframework.jndi.JndiTemplate;
import org.springframework.orm.jpa.JpaTransactionManager;
import org.springframework.orm.jpa.LocalContainerEntityManagerFactoryBean;
import org.springframework.orm.jpa.vendor.HibernateJpaVendorAdapter;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.annotation.EnableTransactionManagement;

import javax.naming.NamingException;
import javax.persistence.EntityManagerFactory;
import javax.sql.DataSource;
import java.util.Properties;
import java.util.ResourceBundle;

/**
 *
 * @author m.kachalov
 */
@Configuration
@EnableTransactionManagement
@EnableJpaRepositories(
        basePackages = {"moscow.ptnl.contingent.nsi.infrastructure.repository"},
        entityManagerFactoryRef = "contingentManagerFactory", 
        transactionManagerRef = "contingentTransactionManager"
)
@PropertySource("classpath:persistence.properties")
public class PersistenceConfiguration {
    
    private static final Logger LOG = LoggerFactory.getLogger(PersistenceConfiguration.class);
    
    public static final String PU_CONTINGENT_NAME = "contingent";
    
    @Value("${datasource.contingent.jndi-name}")
    private String contingentDataSourceJNDIName;
    
    @Bean(name = "contingentDataSource")
    public DataSource dataSource() {
        DataSource dataSource = null;
        try {
            JndiTemplate jndi = new JndiTemplate();
            dataSource = jndi.lookup(contingentDataSourceJNDIName, DataSource.class);
            LOG.info("DataSource init " + PersistenceConfiguration.class);
        } catch (NamingException e) {
            LOG.error("Error DataSource init " + PersistenceConfiguration.class);
        }
        return dataSource;
    }
    
    @Bean(name = "contingentManagerFactory")
    public EntityManagerFactory entityManagerFactory(@Qualifier("contingentDataSource") DataSource dataSource) {
        ResourceBundle bundle = ResourceBundle.getBundle("persistence");
        HibernateJpaVendorAdapter vendorAdapter = new HibernateJpaVendorAdapter();
        vendorAdapter.setShowSql(true);
        vendorAdapter.setGenerateDdl(false);
        LocalContainerEntityManagerFactoryBean factory = new LocalContainerEntityManagerFactoryBean();
        factory.setJpaVendorAdapter(vendorAdapter);
        Properties jpaProperties = new Properties();
        jpaProperties.setProperty("hibernate.jdbc.fetch_size", bundle.getString("org.hibernate.fetchSize"));
        final String prefix = "spring.jpa.properties.";
        bundle.keySet().stream()
                .filter(k -> k.startsWith(prefix))
                .map(k -> k.substring(prefix.length()))
                .forEach(k -> jpaProperties.setProperty(k, bundle.getString(prefix + k)));        
        factory.setJpaProperties(jpaProperties);
        factory.setPackagesToScan("moscow.ptnl");
        factory.setDataSource(dataSource);
        factory.setPersistenceUnitName(PU_CONTINGENT_NAME);
        factory.afterPropertiesSet();
        return factory.getObject();
    }
    
    @Bean (name = "contingentTransactionManager")
    @Primary
    public PlatformTransactionManager transactionManager(@Qualifier("contingentManagerFactory") EntityManagerFactory entityManagerFactory) {
        JpaTransactionManager txManager = new JpaTransactionManager();
        txManager.setEntityManagerFactory(entityManagerFactory);
        return txManager;
    }

}
