package moscow.ptnl.contingent.nsi;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.builder.SpringApplicationBuilder;
import org.springframework.boot.web.servlet.support.SpringBootServletInitializer;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.FilterType;

/**
 *
 * @author m.kachalov
 */
@SpringBootApplication()
@ComponentScan(basePackages = "moscow.ptnl",
        //Из area-infrastructure нам надо только репозитории, остальное исключаем
        excludeFilters = { @ComponentScan.Filter(type = FilterType.REGEX, pattern = "moscow.ptnl.contingent.area.*"),
                @ComponentScan.Filter(type = FilterType.REGEX, pattern = "moscow.ptnl.contingent.esuInputTasks.*"),
                @ComponentScan.Filter(type = FilterType.REGEX, pattern = "moscow.ptnl.contingent.scheduler.*"),
                @ComponentScan.Filter(type = FilterType.REGEX, pattern = "moscow.ptnl.contingent.service.*")})
public class Application extends SpringBootServletInitializer {
    
    public static void main(String[] args) {
        SpringApplication.run(Application.class, args);
    }

    @Override
    protected SpringApplicationBuilder configure(SpringApplicationBuilder application) {
        return application.sources(Application.class);
    }
    
}
