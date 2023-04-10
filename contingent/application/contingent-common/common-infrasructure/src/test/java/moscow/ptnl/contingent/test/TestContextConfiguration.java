package moscow.ptnl.contingent.test;

import jakarta.annotation.PostConstruct;
import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import java.sql.SQLException;
import java.io.File;
import java.io.FileNotFoundException;
import java.util.ResourceBundle;
import javax.sql.DataSource;
import liquibase.Liquibase;
import liquibase.database.Database;
import liquibase.database.DatabaseFactory;
import liquibase.database.jvm.JdbcConnection;
import liquibase.exception.LiquibaseException;
import liquibase.resource.DirectoryResourceAccessor;
import moscow.ptnl.contingent.PersistenceConstraint;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.ComponentScans;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Lazy;
import org.springframework.context.annotation.Primary;
import org.springframework.jdbc.datasource.DriverManagerDataSource;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.support.TransactionTemplate;

/**
 *
 * @author m.kachalov
 */
@Configuration
@ComponentScans({
    @ComponentScan("moscow.ptnl.contingent")        
})
public class TestContextConfiguration {
    
    @PersistenceContext(unitName = PersistenceConstraint.PU_CONTINGENT_NAME)
    protected EntityManager entityManager;

    @Autowired @Lazy
    private TransactionTemplate transactionTemplate;

    @Autowired
    private DataSource dataSource;
    
    @Bean
    public TransactionTemplate transactionTemplate(PlatformTransactionManager transactionManager) {
        return new TransactionTemplate(transactionManager);
    }
    
    @Bean(name = PersistenceConstraint.DS_BEAN_NAME) @Primary
    public DataSource dataSource() {
        ResourceBundle bundle = ResourceBundle.getBundle("persistence-test");
        DriverManagerDataSource dataSource = new DriverManagerDataSource();
        dataSource.setDriverClassName(bundle.getString("jdbc.driverClassName"));
        dataSource.setUrl(bundle.getString("jdbc.url"));
        return dataSource;
    }

    @PostConstruct
    public void init() throws LiquibaseException, SQLException, FileNotFoundException {
        Database database = DatabaseFactory.getInstance().findCorrectDatabaseImplementation(new JdbcConnection(dataSource.getConnection()));
        Liquibase liquibase = new Liquibase(
                "changelog/area/versions/master.xml", 
                new DirectoryResourceAccessor(new File("../../database")), 
                database
        );
        liquibase.dropAll();
        liquibase.update("");
        //Выключение режима Oracle, иначе не работают Sequence
        transactionTemplate.executeWithoutResult(status -> entityManager.createNativeQuery("set mode Oracle;").executeUpdate());
    }
    
}
