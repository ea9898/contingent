/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package area.service.security;

import moscow.ptnl.contingent.service.security.SecuritySettingService;
import moscow.ptnl.contingent.service.setting.SettingService;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 *
 * @author m.kachalov
 */
@Configuration
public class MockConfiguration {
    
    @MockBean
    private SettingService settingService;
    
    @Bean
    public SecuritySettingService securitySettingService() {
        return new SecuritySettingService();
    }
}
