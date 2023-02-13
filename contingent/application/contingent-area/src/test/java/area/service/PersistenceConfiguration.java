package area.service;

import java.util.Properties;
import java.util.ResourceBundle;
import jakarta.persistence.EntityManagerFactory;
import javax.sql.DataSource;

import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.ComponentScans;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;
import org.springframework.context.annotation.PropertySource;
import org.springframework.jdbc.datasource.DriverManagerDataSource;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;
import org.springframework.orm.jpa.JpaTransactionManager;
import org.springframework.orm.jpa.LocalContainerEntityManagerFactoryBean;
import org.springframework.orm.jpa.vendor.HibernateJpaVendorAdapter;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.annotation.EnableTransactionManagement;

/**
 *
 * @author sorlov
 * Настройка базы данных в памяти для тестов
 */
@Configuration
@EnableTransactionManagement
@EnableJpaRepositories(
        basePackages = {"moscow.ptnl.contingent.repository", "moscow.ptnl.contingent.nsi.repository"},
        entityManagerFactoryRef = "contingentManagerFactory", 
        transactionManagerRef = "contingentTransactionManager"
)
@ComponentScans({@ComponentScan("moscow.ptnl.contingent.repository"), @ComponentScan("moscow.ptnl.contingent.nsi.repository")})
@PropertySource("classpath:persistence.properties")
public class PersistenceConfiguration {
    
    private static final String PU_CONTINGENT_NAME = "contingent";

    @Bean(name = "contingentDataSource")
    public DataSource dataSource() {
        ResourceBundle bundle = ResourceBundle.getBundle("persistence");
        DriverManagerDataSource dataSource = null;
        dataSource = new DriverManagerDataSource();
        dataSource.setDriverClassName(bundle.getString("jdbc.driverClassName"));
        dataSource.setUrl(bundle.getString("jdbc.url"));
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
