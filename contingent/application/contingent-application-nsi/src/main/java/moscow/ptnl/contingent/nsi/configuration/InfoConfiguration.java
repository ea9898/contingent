package moscow.ptnl.contingent.nsi.configuration;

import org.springframework.boot.web.servlet.ServletRegistrationBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.context.WebApplicationContext;

import jakarta.servlet.Servlet;
import moscow.ptnl.mod.health.servlet.InfoServlet;

/**
 *
 * @author m.kachalov
 */
@Configuration
public class InfoConfiguration {
    
    @Bean
    public ServletRegistrationBean<Servlet> infoServletRegistrationBean(WebApplicationContext webApplicationContext) {
        ServletRegistrationBean<Servlet> regBeen = new ServletRegistrationBean<>(new InfoServlet(),
                "/info.json");
        regBeen.setLoadOnStartup(2);

        return regBeen;
    }
    
}
