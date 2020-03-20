/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package form;

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
