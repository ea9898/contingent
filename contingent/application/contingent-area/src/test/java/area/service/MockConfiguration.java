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
import moscow.ptnl.contingent.domain.AreaErrorReason;
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
import moscow.ptnl.contingent.error.ContingentException;
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
    public Algorithms algorithms() {
        return new Algorithms(Mockito.mock(AlgorithmsHelper.class));
    }

    @Bean
    public MappingDomainServiceImpl mappingDomain() {
        return new MappingDomainServiceImpl();
    }

    @Bean
    public NsiFormServiceHelper nsiFormServiceHelper() {
        return new NsiFormServiceHelperImpl();
    }

    @Bean
    public FormService formService() {
        return new FormService();
    }

    @Bean
    public NsiFormResponseMapper areaHelper1() {
        return new NsiFormResponseMapperImpl();
    }

    @Bean
    public FormServicePortType formServicePortType() {
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
                PhpSphinxSearchFromGlobalIdResponse search = new PhpSphinxSearchFromGlobalIdResponse();
                if (body.getGlobalId() == -999990077) {
                    throw new Fault("Адрес не найден");
                }

                search.setOut("" +
                        "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>" +
                        "<response count=\"31\">" +
                        "<responseEntity form_id=\"127\" field_id=\"813\" >" +
                        "<REGION_ID>67200856</REGION_ID>" +
                        "<REGION_TYPENAME_SHORT>г.</REGION_TYPENAME_SHORT>" +
                        "<REGION_AOGUID>0c5b2444-70a0-4932-980c-b4dc0d3f02b5</REGION_AOGUID>" +
                        "<REGION_NAME>Москва</REGION_NAME>" +
                        "<REGION_TYPENAME>Город</REGION_TYPENAME>" +

                        "<multifield field_id=\"813\">" +
                        "<field>" +
                        "<PLACECODE xsi:nil = \"true\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"/>" +
                        "<REGION_TE_CODE><value>0100</value></REGION_TE_CODE>" +
                        "<CITYCODE xsi:nil = \"true\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"/>" +
                        "<PLANCODE xsi:nil = \"true\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"/>" +
                        "<STREETCODE xsi:nil = \"true\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"/>" +
                        "<ADDRESS>муниципальный округ Тверской</ADDRESS>" +
                        "<KOD_GIVZ xsi:nil = \"true\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"/>" +
                        "<KLADR xsi:nil = \"true\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"/>" +
                        "<AOGUID xsi:nil = \"true\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"/>" +
                        "<AREA_TE_TYPENAME>муниципальный округ</AREA_TE_TYPENAME>" +
                        "<AREA_TE_TE_CODE>0108</AREA_TE_TE_CODE>" +
                        "<REGIONCODE>77</REGIONCODE>" +
                        "<AREACODE_OMK_TE><value>0108</value></AREACODE_OMK_TE>" +
                        "<AREACODE xsi:nil = \"true\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"/>" +
                        "<AREA_TE_NAME>Тверской</AREA_TE_NAME>" +
                        "<AOLEVEL>25</AOLEVEL>" +
                        "<POSTALCODE xsi:nil = \"true\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"/>" +
                        "<AREA_TE_ID>67186498</AREA_TE_ID>" +
                        "<GLOBAL_ID>67186498</GLOBAL_ID>" +
                        "<GLOBAL_ID_NEW xsi:nil = \"true\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"/>" +
                        "<IS_DELETED>0</IS_DELETED>" +
                        "</field>" +
                        "</multifield>" +

                        "<multifield field_id=\"816\">" +
                        "<field>" +
                        "<REGION_TE_TYPENAME>административный округ</REGION_TE_TYPENAME>" +
                        "<REGION_TE_NAME>Центральный</REGION_TE_NAME>" +
                        "<REGION_TE_NAME_SHORT>ЦАО</REGION_TE_NAME_SHORT>" +
                        "<REGION_TE_TE_CODE>0100</REGION_TE_TE_CODE>" +
                        "<REGION_TE_ID>67186390</REGION_TE_ID>" +
                        "</field>" +
                        "</multifield>" +

                        "</responseEntity>" +
                        "</response>");

                return search;
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
    public AreaHelper areaHelper() {
        return new AreaHelper();
    }

    @Bean
    public MedicalEmployeeHelper medicalEmployeeHelper() {
        return new MedicalEmployeeHelper();
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
    public MainEmployeesMapper mainEmployeesMapper() {
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
    public SettingService settingService() {
        return new SettingServiceImpl();
    }

    @Bean
    public OrderService orderService() {
        return new OrderServiceImpl();
    }

    @MockBean
    public HistoryService historyService;

    @Bean
    public AddressMapper addressMapper() {
        return new AddressMapperImpl();
    }

    @Bean
    public AreaService areaService() {
        return new AreaServiceImpl();
    }

    @Bean
    public AreaPT areaPTv3() {
        return new moscow.ptnl.contingent.area.ws.v3.AreaServiceImpl();
    }

    @Bean
    public SoapBaseExceptionMapper mapper() {
        return new SoapExceptionMapper();
    }

    @MockBean
    public ru.mos.emias.contingent2.area.AreaPT areaPT;

    @MockBean
    public ru.mos.emias.contingent2.area.v2.AreaPT areaPTv2;

    @Bean
    public UserContextMapper userContextMapper() {
        return new UserContextMapperImpl();
    }

    @Bean
    public SoapCustomMapperV3 soapCustomMapperV3() {
        return new SoapCustomMapperV3();
    }

    @Bean
    public MoMuService moMuService() {
        return new MoMuServiceImpl();
    }

    @Bean
    public AreaMapperV3 areaMapperV3() {
        return new AreaMapperV3();
    }

    @Bean
    public AreaTypeShortMapperV3 areaTypeShortMapperV3() {
        return new AreaTypeShortMapperV3Impl();
    }

    @Bean
    public AreaMedicalEmployeeMapperV3 areaMedicalEmployeeMapperV3() {
        return new AreaMedicalEmployeeMapperV3();
    }

    @Bean
    public CodeNameTypeMapperV3 codeNameTypeMapperV3() {
        return new CodeNameTypeMapperV3();
    }

    @Bean
    public MuAvailableAreaTypes2Mapper muAvailableAreaTypes2Mapper() {
        return new MuAvailableAreaTypes2MapperImpl();
    }

    @Bean
    public AreaBriefMapperV3 areaBriefMapperV3() {
        return new AreaBriefMapperV3Impl();
    }

    @Bean
    public SearchAreaAddressMapperV3 searchAreaAddressMapperV3() {
        return new SearchAreaAddressMapperV3Impl();
    }

    @Bean
    public MuAvailableAreaTypesInMoMapper muAvailableAreaTypesInMoMapper() {
        return new MuAvailableAreaTypesInMoMapper();
    }

    @Bean
    public GetAreaHistoryMapperV3 getAreaHistoryMapperV3() {
        return new GetAreaHistoryMapperV3Impl();
    }

    @Bean
    public AddMedicalEmployeeMapperV3 addMedicalEmployeeMapperV3() {
        return new AddMedicalEmployeeMapperV3Impl();
    }

    @Bean
    public ChangeMedicalEmployeeMapperV3 changeMedicalEmployeeMapperV3() {
        return new ChangeMedicalEmployeeMapperV3Impl();
    }

    @Bean
    public AddressRegistryToAddressRegistryBaseMapperV3 addressRegistryToAddressRegistryBaseMapperV3() {
        return new AddressRegistryToAddressRegistryBaseMapperV3Impl();
    }

    @Bean
    public AreaDnMapperV3 areaDnMapperV3() {
        return new AreaDnMapperV3();
    }

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
