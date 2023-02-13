package moscow.ptnl.test.form;

import moscow.ptnl.contingent.nsi.service.NsiFormServiceHelper;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 *
 * @author mkachalov
 */
@Configuration
public class MockConfiguration {
    
    @Bean
    public NsiFormServiceHelper nsiFormServiceHelper() {
        return new NsiFormServiceHelper();
    }
    
}
