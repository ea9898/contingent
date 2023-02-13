package moscow.ptnl.contingent.sysop.configuration;

import jakarta.servlet.RequestDispatcher;
import java.util.ArrayList;
import java.util.Arrays;
import org.apache.cxf.Bus;
import org.apache.cxf.bus.spring.SpringBus;
import org.apache.cxf.ext.logging.LoggingFeature;
import org.apache.cxf.transport.servlet.CXFServlet;
import org.springframework.boot.web.servlet.ServletRegistrationBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 *
 * @author m.kachalov
 */
@Configuration
public class CXFConfiguration {
    
    @Bean(name = Bus.DEFAULT_BUS_ID)
    public SpringBus springBus(LoggingFeature loggingFeature) {
        SpringBus bus = new SpringBus();
        bus.setFeatures(new ArrayList<>(Arrays.asList(loggingFeature)));
        return bus;
    }
    
    @Bean
    public LoggingFeature loggingFeature() {
        LoggingFeature loggingFeature = new LoggingFeature();
        loggingFeature.setLimit(-1); //The default is 48 * 1024 https://redmine.ptnl.moscow/issues/30432
        return loggingFeature;
    }
    
    @Bean
    public ServletRegistrationBean<CXFServlet> cxfServletRegistrationBean() {        
        ServletRegistrationBean<CXFServlet> regBeen = new ServletRegistrationBean<>(new CXFServlet(), "/*");
        regBeen.setLoadOnStartup(1);
        
        //Настройка редиректа для файла info.json
        //cXFServlet.setInitParameter("redirects-list", "/info.json");
        //cXFServlet.setInitParameter("redirect-attributes", RequestDispatcher.INCLUDE_REQUEST_URI);
        //cXFServlet.setInitParameter("redirect-servlet-name", "default");
        
        return regBeen;
    }
    
}
