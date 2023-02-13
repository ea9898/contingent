package moscow.ptnl.contingent.area.configuration;

import jakarta.persistence.EntityManager;
import jakarta.servlet.Servlet;
import java.util.Arrays;
import java.util.List;
import moscow.ptnl.mod.health.repository.DatabaseCheckRepository;
import moscow.ptnl.mod.health.service.HealthCheckService;
import moscow.ptnl.mod.health.servlet.HealthCheckServlet;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.web.servlet.ServletRegistrationBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.PropertySource;

/**
 *
 * @author m.kachalov
 */
@Configuration
@PropertySource("classpath:application.properties")
public class HealthConfiguration {
    
    @Value("${health.check.tables}")
    private String tables;
    
    @Bean      
    public ServletRegistrationBean<Servlet> healthServletRegistrationBean(HealthCheckService service) {
        
        List<String> tablesList = Arrays.asList(tables.split(","));
        ServletRegistrationBean<Servlet> regBeen = new ServletRegistrationBean<>(new HealthCheckServlet(service, tablesList),
                "/health");
        regBeen.setLoadOnStartup(3);

        return regBeen;
    }
    
    @Bean
    public DatabaseCheckRepository databaseCheckRepository(EntityManager em) {
        return new DatabaseCheckRepository() {
            @Override
            public EntityManager getEntityManager() {
                return em;
            }
        };
    }
    
}
