/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package area.service;

import moscow.ptnl.contingent.domain.area.Algorithms;
import moscow.ptnl.contingent.domain.area.AlgorithmsHelper;
import moscow.ptnl.contingent.area.service.AreaAddressChecker;
import moscow.ptnl.contingent.area.service.AreaServiceHelper;
import moscow.ptnl.contingent.domain.area.AreaServiceInternalAsyncImpl;
import moscow.ptnl.contingent.area.service.EsuHelperServiceImpl;
import moscow.ptnl.contingent.area.service.HistoryServiceHelperImpl;
import moscow.ptnl.contingent.area.transform.AreaAddressMapper;
import moscow.ptnl.contingent.infrastructure.service.TransactionRunService;
import moscow.ptnl.contingent.infrastructure.service.setting.SettingService;
import moscow.ptnl.contingent.area.service.interceptor.LogESUInterceptor;
import moscow.ptnl.contingent.area.transform.AddressMapper;
import moscow.ptnl.contingent.area.transform.AddressRegistryBaseTypeCloner;
import moscow.ptnl.contingent.area.transform.AreaAddressClone;
import moscow.ptnl.contingent.area.transform.AreaMedicalEmployeesClone;
import moscow.ptnl.contingent.area.transform.SearchAreaAddressCloner;
import moscow.ptnl.contingent.util.XMLGregorianCalendarMapper;
import moscow.ptnl.contingent.area.transform.model.esu.AddressesMapper;
import moscow.ptnl.contingent.area.transform.model.esu.AreaInfoEventMapper;
import moscow.ptnl.contingent.area.transform.model.esu.AreaRestrictionMapper;
import moscow.ptnl.contingent.area.transform.model.esu.AttachOnAreaChangeMapper;
import moscow.ptnl.contingent.area.transform.model.esu.MainEmployeesMapper;
import moscow.ptnl.contingent.area.transform.model.esu.ReplacementEmployeesMapper;
import moscow.ptnl.contingent.area.configuration.EventChannelsConfiguration;
import moscow.ptnl.contingent.area.endpoint.ESUEventEndpoint;
import moscow.ptnl.contingent.infrastructure.service.EsuService;
import moscow.ptnl.contingent.domain.area.HistoryService;
import org.mockito.Mockito;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.boot.test.mock.mockito.SpyBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.EnableAspectJAutoProxy;
import org.springframework.integration.channel.DirectChannel;
import org.springframework.integration.config.EnableIntegration;
import org.springframework.messaging.MessageChannel;

/**
 *
 * @author mkachalov
 */
@Configuration
@EnableIntegration
@EnableAspectJAutoProxy
public class MockConfiguration {
    
    @Bean
    public EsuHelperServiceImpl esuHelperService() {
        return new EsuHelperServiceImpl();
    }
    
    @Bean
    public Algorithms algorithms(){
        return new Algorithms(Mockito.mock(AlgorithmsHelper.class));
    }

    @MockBean
    public AlgorithmsHelper algorithmsHelper;
    
    @Bean
    public AreaInfoEventMapper areaInfoEventMapper() {
        return new AreaInfoEventMapper();
    }
    
    @Bean
    public XMLGregorianCalendarMapper getXMLGregorianCalendarMapper() {
        return new XMLGregorianCalendarMapper();
    }
    
    @Bean
    public AttachOnAreaChangeMapper attachOnAreaChangeMapper() {
        return new AttachOnAreaChangeMapper();
    }
    
    @Bean
    public AreaRestrictionMapper areaRestrictionMapper() {
        return new AreaRestrictionMapper();
    }
    
    @Bean
    public MainEmployeesMapper mainEmployeesMapper(){
        return new MainEmployeesMapper();
    }
    
    @Bean
    public ReplacementEmployeesMapper replacementEmployeesMapper() {
        return new ReplacementEmployeesMapper();
    }
    
    @Bean
    public AddressesMapper addressesMapper() {
        return new AddressesMapper();
    }

    @Bean(name = EventChannelsConfiguration.ESU_EVENT_CHANNEL_NAME)
    public MessageChannel esuChannel() {
        return new DirectChannel();
    }
    
    @Bean
    public ESUEventEndpoint getESUEventEndpoint() {
        return new ESUEventEndpoint();
    }
    
    @Bean
    public EsuService esuService() {
        return new MockEsuService();
    }

    @Bean
    public LogESUInterceptor logESUInterceptor() {
        return new LogESUInterceptor();
    }

    @SpyBean
    public AreaServiceHelper areaServiceHelper;
    
    @MockBean
    public AreaAddressChecker areaAddressChecker;
    
    @MockBean
    public SettingService settingService;

    @MockBean
    public HistoryService historyService;

    @MockBean
    public AddressMapper addressMapper;

    @MockBean
    public AddressRegistryBaseTypeCloner addressRegistryBaseTypeCloner;

    @MockBean
    public AreaServiceInternalAsyncImpl asyncService;

    @MockBean
    public SearchAreaAddressCloner searchAreaAddressCloner;

    @MockBean
    AreaAddressClone areaAddressClone;

    @MockBean
    private AreaMedicalEmployeesClone areaMedicalEmployeesClone;

    @MockBean
    private HistoryServiceHelperImpl historyServiceHelperImpl;

    @MockBean
    private TransactionRunService transactionRunService;

    @MockBean
    private AreaAddressMapper areaAddressMapper;
}
