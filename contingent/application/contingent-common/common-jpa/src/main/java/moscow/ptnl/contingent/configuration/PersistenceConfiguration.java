package moscow.ptnl.contingent.configuration;

import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;
import java.util.Properties;
import java.util.ResourceBundle;
import jakarta.persistence.EntityManagerFactory;
import javax.sql.DataSource;
import moscow.ptnl.contingent.PersistenceConstraint;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;
import org.springframework.context.annotation.PropertySource;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;
import org.springframework.orm.jpa.JpaTransactionManager;
import org.springframework.orm.jpa.LocalContainerEntityManagerFactoryBean;
import org.springframework.orm.jpa.vendor.HibernateJpaVendorAdapter;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.annotation.EnableTransactionManagement;

/**
 *
 * @author m.kachalov
 */
@Configuration("CommonPersistenceConfiguration")
@EnableTransactionManagement
@EnableJpaRepositories(
        basePackages = { 
            "moscow.ptnl.mod",
            "moscow.ptnl.contingent.repository",
            "moscow.ptnl.contingent.nsi.repository", 
            "moscow.ptnl.contingent.area.repository"
        },
        entityManagerFactoryRef = "contingentManagerFactory", 
        transactionManagerRef = "contingentTransactionManager"
)
@PropertySource("classpath:persistence.properties")
public class PersistenceConfiguration {
    
    private static final Logger LOG = LoggerFactory.getLogger(PersistenceConfiguration.class);
        
    //@Value("${datasource.contingent.jndi-name}")
    //private String contingentDataSourceJNDIName;
    
    @Value("${spring.datasource.driver-class-name}")
    private String driver;

    @Value("${spring.datasource.url}")
    private String datasourceUrl;

    @Value("${spring.datasource.username}")
    private String username;

    @Value("${spring.datasource.password}")
    private String password;

    @Value("${spring.datasource.hikari.maximum-pool-size}")
    private Integer maxPoolSize;

    @Value("${spring.datasource.hikari.maxIdleTime}")
    private Integer idleTimeout;

    @Value("${spring.datasource.hikari.maxTimeout}")
    private Integer maxLifetime;

    @Value("${spring.jpa.properties.hibernate.show-sql}")
    private Boolean showSql;
    
    //@Bean(name = "contingentDataSource")
    //public DataSource dataSource() {
    //    DataSource dataSource = null;
    //    try {
    //        JndiTemplate jndi = new JndiTemplate();
    //        dataSource = jndi.lookup(contingentDataSourceJNDIName, DataSource.class);
    //        LOG.info("DataSource init " + PersistenceConfiguration.class);
    //    } catch (NamingException e) {
    //        LOG.error("Error DataSource init " + PersistenceConfiguration.class);
    //    }
    //    return dataSource;
    //}
    
    @Bean(name = PersistenceConstraint.DS_BEAN_NAME)
    public DataSource dataSource() {
        HikariConfig hikariConfig = new HikariConfig();
        hikariConfig.setDriverClassName(driver);
        hikariConfig.setJdbcUrl(datasourceUrl);
        hikariConfig.setUsername(username);
        hikariConfig.setPassword(password);
        hikariConfig.setMaximumPoolSize(maxPoolSize);
        hikariConfig.setConnectionTestQuery("SELECT 1");
        hikariConfig.setIdleTimeout(idleTimeout);
        hikariConfig.setMaxLifetime(maxLifetime);
        return new HikariDataSource(hikariConfig);
    }
    
    @Bean(name = PersistenceConstraint.MF_BEAN_NAME)
    public EntityManagerFactory entityManagerFactory(@Qualifier(PersistenceConstraint.DS_BEAN_NAME) DataSource dataSource) {
        ResourceBundle bundle = ResourceBundle.getBundle("persistence");
        HibernateJpaVendorAdapter vendorAdapter = new HibernateJpaVendorAdapter();
        vendorAdapter.setShowSql(showSql);
        vendorAdapter.setGenerateDdl(false);
        LocalContainerEntityManagerFactoryBean factory = new LocalContainerEntityManagerFactoryBean();
        factory.setJpaVendorAdapter(vendorAdapter);
        Properties jpaProperties = new Properties();
        final String prefix = "spring.jpa.properties.";
        bundle.keySet().stream()
                .filter(k -> k.startsWith(prefix))
                .map(k -> k.substring(prefix.length()))
                .forEach(k -> jpaProperties.setProperty(k, bundle.getString(prefix + k)));        
        factory.setJpaProperties(jpaProperties);
        factory.setPackagesToScan("moscow.ptnl");
        factory.setDataSource(dataSource);
        factory.setPersistenceUnitName(PersistenceConstraint.PU_CONTINGENT_NAME);
        factory.afterPropertiesSet();
        return factory.getObject();
    }
    
    @Bean(name = PersistenceConstraint.TM_BEAN_NAME)
    @Primary
    public PlatformTransactionManager transactionManager(@Qualifier(PersistenceConstraint.MF_BEAN_NAME) EntityManagerFactory entityManagerFactory) {
        JpaTransactionManager txManager = new JpaTransactionManager();
        txManager.setEntityManagerFactory(entityManagerFactory);
        return txManager;
    }

}
