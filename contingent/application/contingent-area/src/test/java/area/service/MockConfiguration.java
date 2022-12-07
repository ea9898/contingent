/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package area.service;

import moscow.ptnl.contingent.area.transform.SoapBaseExceptionMapper;
import moscow.ptnl.contingent.area.transform.SoapVersioningMapper;
import moscow.ptnl.contingent.area.transform.Transformer;
import moscow.ptnl.contingent.area.transform.UserContextMapper;
import moscow.ptnl.contingent.area.transform.UserContextMapperImpl;
import moscow.ptnl.contingent.area.transform.v1.SoapCustomMapper;
import moscow.ptnl.contingent.area.transform.v3.AddMedicalEmployeeMapperV3;
import moscow.ptnl.contingent.area.transform.v3.AddMedicalEmployeeMapperV3Impl;
import moscow.ptnl.contingent.area.transform.v3.AddressRegistryToAddressRegistryBaseMapperV3;
import moscow.ptnl.contingent.area.transform.v3.AddressRegistryToAddressRegistryBaseMapperV3Impl;
import moscow.ptnl.contingent.area.transform.v3.AreaBriefMapperV3;
import moscow.ptnl.contingent.area.transform.v3.AreaBriefMapperV3Impl;
import moscow.ptnl.contingent.area.transform.v3.AreaDnMapperV3;
import moscow.ptnl.contingent.area.transform.v3.AreaMapperV3;
import moscow.ptnl.contingent.area.transform.v3.AreaMedicalEmployeeMapperV3;
import moscow.ptnl.contingent.area.transform.v3.AreaTypeShortMapperV3;
import moscow.ptnl.contingent.area.transform.v3.AreaTypeShortMapperV3Impl;
import moscow.ptnl.contingent.area.transform.v3.ChangeMedicalEmployeeMapperV3;
import moscow.ptnl.contingent.area.transform.v3.ChangeMedicalEmployeeMapperV3Impl;
import moscow.ptnl.contingent.area.transform.v3.CodeNameTypeMapperV3;
import moscow.ptnl.contingent.area.transform.v3.GetAreaHistoryMapperV3;
import moscow.ptnl.contingent.area.transform.v3.GetAreaHistoryMapperV3Impl;
import moscow.ptnl.contingent.area.transform.v3.MuAvailableAreaTypes2Mapper;
import moscow.ptnl.contingent.area.transform.v3.MuAvailableAreaTypes2MapperImpl;
import moscow.ptnl.contingent.area.transform.v3.MuAvailableAreaTypesInMoMapper;
import moscow.ptnl.contingent.area.transform.v3.SearchAreaAddressMapperV3;
import moscow.ptnl.contingent.area.transform.v3.SearchAreaAddressMapperV3Impl;
import moscow.ptnl.contingent.area.transform.v3.SoapCustomMapperV3;
import moscow.ptnl.contingent.area.transform.v3.SoapExceptionMapper;
import moscow.ptnl.contingent.domain.area.MappingDomainServiceImpl;
import moscow.ptnl.contingent.area.service.NsiFormServiceHelperImpl;
import moscow.ptnl.contingent.area.transform.NsiFormResponseMapperImpl;
import moscow.ptnl.contingent.domain.area.Algorithms;
import moscow.ptnl.contingent.domain.area.AlgorithmsHelper;
import moscow.ptnl.contingent.area.service.AreaAddressChecker;
import moscow.ptnl.contingent.domain.area.AreaService;
import moscow.ptnl.contingent.domain.area.AreaServiceImpl;
import moscow.ptnl.contingent.domain.area.AreaServiceInternalAsyncImpl;
import moscow.ptnl.contingent.domain.area.MoMuService;
import moscow.ptnl.contingent.domain.area.MoMuServiceImpl;
import moscow.ptnl.contingent.domain.area.NsiFormResponseMapper;
import moscow.ptnl.contingent.domain.area.OrderServiceImpl;
import moscow.ptnl.contingent.domain.area.heplers.MedicalEmployeeHelper;
import moscow.ptnl.contingent.domain.area.repository.HistoryEventRepository;
import moscow.ptnl.contingent.domain.area.heplers.NsiFormServiceHelper;
import moscow.ptnl.contingent.domain.area.transform.AddressMapperImpl;
import moscow.ptnl.contingent.infrastructure.service.setting.SettingServiceImpl;
import moscow.ptnl.contingent.service.esu.EsuHelperServiceImpl;
import moscow.ptnl.contingent.area.service.HistoryServiceHelperImpl;
import moscow.ptnl.contingent.area.transform.v1.AreaAddressMapper;
import moscow.ptnl.contingent.domain.area.OrderService;
import moscow.ptnl.contingent.domain.area.heplers.AreaHelper;
import moscow.ptnl.contingent.infrastructure.service.TransactionRunService;
import moscow.ptnl.contingent.infrastructure.service.setting.SettingService;
import moscow.ptnl.contingent.area.service.interceptor.LogESUInterceptor;
import moscow.ptnl.contingent.domain.area.transform.AddressMapper;
import moscow.ptnl.contingent.area.transform.v1.AddressRegistryBaseTypeCloner;
import moscow.ptnl.contingent.area.transform.v1.AreaAddressClone;
import moscow.ptnl.contingent.area.transform.v1.AreaMedicalEmployeesClone;
import moscow.ptnl.contingent.area.transform.v1.SearchAreaAddressCloner;
import moscow.ptnl.contingent.util.XMLGregorianCalendarMapper;
import moscow.ptnl.contingent.transform.AddressesMapper;
import moscow.ptnl.contingent.transform.AreaInfoEventMapper;
import moscow.ptnl.contingent.transform.AreaRestrictionMapper;
import moscow.ptnl.contingent.transform.AttachOnAreaChangeMapper;
import moscow.ptnl.contingent.transform.MainEmployeesMapper;
import moscow.ptnl.contingent.transform.ReplacementEmployeesMapper;
import moscow.ptnl.contingent.area.configuration.EventChannelsConfiguration;
import moscow.ptnl.contingent.area.endpoint.ESUEventEndpoint;
import moscow.ptnl.contingent.infrastructure.service.EsuService;
import moscow.ptnl.contingent.domain.area.HistoryService;
import org.mockito.Mockito;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.EnableAspectJAutoProxy;
import org.springframework.integration.channel.DirectChannel;
import org.springframework.integration.config.EnableIntegration;
import org.springframework.messaging.MessageChannel;
import ru.mos.emias.contingent2.area.v3.AreaPT;
import ru.mos.emias.formproduct.formservice.v1.Fault;
import ru.mos.emias.formproduct.formservice.v1.FormService;
import ru.mos.emias.formproduct.formservice.v1.FormServicePortType;
import ru.mos.emias.formproduct.formservice.v1.types.GetFieldsByFormIdRequest;
import ru.mos.emias.formproduct.formservice.v1.types.GetFieldsByFormIdResponse;
import ru.mos.emias.formproduct.formservice.v1.types.GetFormsRequest;
import ru.mos.emias.formproduct.formservice.v1.types.GetFormsResponse;
import ru.mos.emias.formproduct.formservice.v1.types.PhpSphinxSearchFromGlobalIdRequest;
import ru.mos.emias.formproduct.formservice.v1.types.PhpSphinxSearchFromGlobalIdResponse;
import ru.mos.emias.formproduct.formservice.v1.types.PhpSphinxSearchFromGlobalIdXsdRequest;
import ru.mos.emias.formproduct.formservice.v1.types.PhpSphinxSearchFromGlobalIdXsdResponse;
import ru.mos.emias.formproduct.formservice.v1.types.PhpSphinxSearchRequest;
import ru.mos.emias.formproduct.formservice.v1.types.PhpSphinxSearchResponse;
import ru.mos.emias.formproduct.formservice.v1.types.SubscribeFormRequest;
import ru.mos.emias.formproduct.formservice.v1.types.SubscribeFormResponse;
import ru.mos.emias.system.v1.usercontext.UserContext;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;
import java.io.ByteArrayInputStream;
import java.nio.charset.StandardCharsets;

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

    @Bean
    public MappingDomainServiceImpl mappingDomain(){
        return new MappingDomainServiceImpl();
    }

    @Bean
    public NsiFormServiceHelper nsiFormServiceHelper(){
        return new NsiFormServiceHelperImpl();
    }

    @Bean
    public FormService formService(){
        return new FormService();
    }

    @Bean
    public NsiFormResponseMapper areaHelper1(){
        return new NsiFormResponseMapperImpl();
    }

    @Bean
    public FormServicePortType formServicePortType(){
        return new FormServicePortType() {
            @Override
            public GetFormsResponse getForms(GetFormsRequest body, UserContext userContext) throws Fault {
                return new GetFormsResponse();
            }

            @Override
            public GetFieldsByFormIdResponse getFieldsByFormId(GetFieldsByFormIdRequest body, UserContext userContext) throws Fault {
                return new GetFieldsByFormIdResponse();
            }

            @Override
            public PhpSphinxSearchFromGlobalIdResponse searchByGlobalId(PhpSphinxSearchFromGlobalIdRequest body, UserContext userContext) throws Fault {
                return new PhpSphinxSearchFromGlobalIdResponse();
            }

            @Override
            public PhpSphinxSearchResponse searchByText(PhpSphinxSearchRequest body, UserContext userContext) throws Fault {
                return new PhpSphinxSearchResponse();
            }

            @Override
            public SubscribeFormResponse subscription(SubscribeFormRequest body, UserContext userContext) throws Fault {
                return new SubscribeFormResponse();
            }

            @Override
            public PhpSphinxSearchFromGlobalIdXsdResponse generateXsdEntity(PhpSphinxSearchFromGlobalIdXsdRequest body, UserContext userContext) throws Fault {
                return new PhpSphinxSearchFromGlobalIdXsdResponse();
            }
        };
    }

    @MockBean
    public AlgorithmsHelper algorithmsHelper;

    @Bean
    public AreaInfoEventMapper areaInfoEventMapper() {
        return new AreaInfoEventMapper();
    }

    @Bean
    public AreaHelper areaHelper() { return new AreaHelper(); }

    @Bean
    public MedicalEmployeeHelper medicalEmployeeHelper() { return new MedicalEmployeeHelper(); }

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

    @MockBean
    public HistoryEventRepository historyEventRepository;

    @MockBean
    public AreaAddressChecker areaAddressChecker;
    
    @Bean
    public SettingService settingService() { return new SettingServiceImpl(); }

    @Bean
    public OrderService orderService() { return new OrderServiceImpl(); }

    @MockBean
    public HistoryService historyService;

    @Bean
    public AddressMapper addressMapper() { return new AddressMapperImpl(); }

    @Bean
    public AreaService areaService() { return new AreaServiceImpl(); }

    @Bean
    public AreaPT areaPTv3() { return new moscow.ptnl.contingent.area.ws.v3.AreaServiceImpl(); }

    @Bean
    public SoapBaseExceptionMapper mapper() { return new SoapExceptionMapper(); }

    @MockBean
    public ru.mos.emias.contingent2.area.AreaPT areaPT;

    @MockBean
    public ru.mos.emias.contingent2.area.v2.AreaPT areaPTv2;

    @Bean
    public UserContextMapper userContextMapper() { return new UserContextMapperImpl(); }

    @Bean
    public SoapCustomMapperV3 soapCustomMapperV3() { return new SoapCustomMapperV3(); }

    @Bean
    public MoMuService moMuService() { return new MoMuServiceImpl(); }

    @Bean
    public AreaMapperV3 areaMapperV3() { return new AreaMapperV3(); }

    @Bean
    public AreaTypeShortMapperV3 areaTypeShortMapperV3() { return new AreaTypeShortMapperV3Impl(); }

    @Bean
    public AreaMedicalEmployeeMapperV3 areaMedicalEmployeeMapperV3() { return new AreaMedicalEmployeeMapperV3(); }

    @Bean
    public CodeNameTypeMapperV3 codeNameTypeMapperV3() { return new CodeNameTypeMapperV3(); }

    @Bean
    public MuAvailableAreaTypes2Mapper muAvailableAreaTypes2Mapper() { return new MuAvailableAreaTypes2MapperImpl(); }

    @Bean
    public AreaBriefMapperV3 areaBriefMapperV3() { return new AreaBriefMapperV3Impl(); }

    @Bean
    public SearchAreaAddressMapperV3 searchAreaAddressMapperV3() { return new SearchAreaAddressMapperV3Impl(); }

    @Bean
    public MuAvailableAreaTypesInMoMapper muAvailableAreaTypesInMoMapper() { return new MuAvailableAreaTypesInMoMapper(); }

    @Bean
    public GetAreaHistoryMapperV3 getAreaHistoryMapperV3() { return new GetAreaHistoryMapperV3Impl(); }

    @Bean
    public AddMedicalEmployeeMapperV3 addMedicalEmployeeMapperV3() { return new AddMedicalEmployeeMapperV3Impl(); }

    @Bean
    public ChangeMedicalEmployeeMapperV3 changeMedicalEmployeeMapperV3() { return new ChangeMedicalEmployeeMapperV3Impl(); }

    @Bean
    public AddressRegistryToAddressRegistryBaseMapperV3 addressRegistryToAddressRegistryBaseMapperV3() { return new AddressRegistryToAddressRegistryBaseMapperV3Impl(); }

    @Bean
    public AreaDnMapperV3 areaDnMapperV3() { return new AreaDnMapperV3(); }

    @MockBean
    public moscow.ptnl.contingent.area.transform.v1.PagingOptionsMapper pagingOptionsMapper;

    @MockBean
    public SoapVersioningMapper soapVersioningMapper;

    @MockBean
    public Transformer transformer;

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
