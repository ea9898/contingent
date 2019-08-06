/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package area.service;

import moscow.ptnl.contingent.area.service.Algorithms;
import moscow.ptnl.contingent.area.service.AlgorithmsHelper;
import moscow.ptnl.contingent.area.service.AreaAddressChecker;
import moscow.ptnl.contingent.area.service.AreaServiceHelper;
import moscow.ptnl.contingent.area.service.AreaServiceInternal;
import moscow.ptnl.contingent.area.service.AreaServiceInternalImpl;
import moscow.ptnl.contingent.area.service.EsuHelperService;
import moscow.ptnl.contingent.area.service.SettingService;
import moscow.ptnl.contingent.area.transform.NotNsiAddressMapper;
import moscow.ptnl.contingent.area.transform.NsiAddressMapper;
import moscow.ptnl.contingent.area.transform.model.esu.AreaInfoEventMapper;
import moscow.ptnl.contingent.area.transform.model.esu.AttachOnAreaChangeMapper;
import moscow.ptnl.contingent.configuration.EventChannelsConfiguration;
import moscow.ptnl.contingent.repository.area.AddressAllocationOrderCRUDRepository;
import moscow.ptnl.contingent.repository.area.AddressAllocationOrderRepository;
import moscow.ptnl.contingent.repository.area.AddressesCRUDRepository;
import moscow.ptnl.contingent.repository.area.AddressesRepository;
import moscow.ptnl.contingent.repository.area.AreaAddressCRUDRepository;
import moscow.ptnl.contingent.repository.area.AreaAddressRepository;
import moscow.ptnl.contingent.repository.area.AreaCRUDRepository;
import moscow.ptnl.contingent.repository.area.AreaMedicalEmployeeCRUDRepository;
import moscow.ptnl.contingent.repository.area.AreaMedicalEmployeeRepository;
import moscow.ptnl.contingent.repository.area.AreaRepository;
import moscow.ptnl.contingent.repository.area.AreaToAreaTypeCRUDRepository;
import moscow.ptnl.contingent.repository.area.AreaToAreaTypeRepository;
import moscow.ptnl.contingent.repository.area.MoAddressCRUDRepository;
import moscow.ptnl.contingent.repository.area.MoAddressRepository;
import moscow.ptnl.contingent.repository.area.MoAvailableAreaTypesCRUDRepository;
import moscow.ptnl.contingent.repository.area.MoAvailableAreaTypesRepository;
import moscow.ptnl.contingent.repository.area.MuAddlAreaTypesRepository;
import moscow.ptnl.contingent.repository.area.MuAvailableAreaTypesCRUDRepository;
import moscow.ptnl.contingent.repository.area.MuAvailableAreaTypesRepository;
import moscow.ptnl.contingent.repository.nsi.AddressFormingElementRepository;
import moscow.ptnl.contingent.repository.nsi.AreaPolicyTypesCRUDRepository;
import moscow.ptnl.contingent.repository.nsi.AreaPolicyTypesRepository;
import moscow.ptnl.contingent.repository.nsi.AreaTypeMedicalPositionsRepository;
import moscow.ptnl.contingent.repository.nsi.AreaTypeSpecializationsRepository;
import moscow.ptnl.contingent.repository.nsi.AreaTypesCRUDRepository;
import moscow.ptnl.contingent.repository.nsi.BuildingRegistryRepository;
import moscow.ptnl.contingent.repository.nsi.PolicyTypeRepository;
import moscow.ptnl.contingent.repository.nsi.PositionNomRepository;
import moscow.ptnl.contingent.service.history.HistoryService;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.integration.channel.NullChannel;
import org.springframework.integration.config.EnableIntegration;
import org.springframework.messaging.MessageChannel;

/**
 *
 * @author mkachalov
 */
@Configuration
//@EnableIntegration
public class MockConfiguration {
    
    
    @Bean
    public EsuHelperService esuHelperService() {
        return new EsuHelperService();
    }
    
    @Bean
    public Algorithms algorithms(){
        return new Algorithms(Mockito.mock(AlgorithmsHelper.class));
    }
    
    @MockBean
    public MoAddressRepository moAddressRepository;
    
    @MockBean
    public AddressFormingElementRepository addressFormingElementRepository;
    
    @MockBean
    public AlgorithmsHelper algorithmsHelper;
    
    @MockBean
    public AreaInfoEventMapper areaInfoEventMapper;
    
    @MockBean
    public AttachOnAreaChangeMapper attachOnAreaChangeMapper;
    
    @MockBean
    public AreaAddressRepository areaAddressRepository;
    
    @Bean(name = EventChannelsConfiguration.ESU_EVENT_CHANNEL_NAME)
    public MessageChannel esuChannel() {
        return new NullChannel();
    }
    
    @Bean
    public AreaServiceInternal areaServiceInternal() {
        return new AreaServiceInternalImpl();
    }
    
    @MockBean
    public AreaRepository areaRepository;
    
    @MockBean
    public AreaTypesCRUDRepository areaTypesCRUDRepository;
    
    @MockBean
    public AreaCRUDRepository areaCRUDRepository;
    
    @MockBean
    public AreaToAreaTypeCRUDRepository areaToAreaTypeCRUDRepository;
    
    @MockBean
    public AreaToAreaTypeRepository areaToAreaTypeRepository;
    
    @MockBean
    public AddressAllocationOrderCRUDRepository addressAllocationOrderCRUDRepository;
    
    @MockBean
    public AddressAllocationOrderRepository addressAllocationOrderRepository;
    
    @MockBean
    public AreaMedicalEmployeeCRUDRepository areaMedicalEmployeeCRUDRepository;
    
    @MockBean
    public AreaTypeMedicalPositionsRepository areaTypeMedicalPositionsRepository;
    
    @MockBean
    public AreaMedicalEmployeeRepository areaMedicalEmployeeRepository;
    
    @MockBean
    public BuildingRegistryRepository buildingRegistryRepository;
    
    @MockBean
    public AddressesCRUDRepository addressesCRUDRepository;
    
    @MockBean
    public AddressesRepository addressesRepository;
    
    @MockBean
    public MoAddressCRUDRepository moAddressCRUDRepository;
    
    @MockBean
    public AreaAddressCRUDRepository areaAddressCRUDRepository;
    
    @MockBean
    public MoAvailableAreaTypesCRUDRepository moAvailableAreaTypesCRUDRepository;
    
    @MockBean
    public MoAvailableAreaTypesRepository moAvailableAreaTypesRepository;
    
    @Bean
    public AreaServiceHelper areaServiceHelper(){
        return new AreaServiceHelper();
    }
    
    @MockBean
    public AreaAddressChecker areaAddressChecker;
    
    @MockBean
    public SettingService settingService;
    
    @MockBean
    public MuAvailableAreaTypesCRUDRepository muAvailableAreaTypesCRUDRepository;
    
    @MockBean
    public MuAvailableAreaTypesRepository muAvailableAreaTypesRepository;
    
    @MockBean
    public HistoryService historyService;
    
    @MockBean
    public PositionNomRepository positionNomRepository;
    
    @MockBean
    public AreaTypeSpecializationsRepository areaTypeSpecializationsRepository;
    
    @MockBean
    public AreaPolicyTypesCRUDRepository areaPolicyTypesCRUDRepository;
    
    @MockBean
    public AreaPolicyTypesRepository areaPolicyTypesRepository;
    
    @MockBean
    public PolicyTypeRepository policyTypeRepository;
    
    @MockBean
    public MuAddlAreaTypesRepository muAddlAreaTypesRepository;
    
    @MockBean
    public NsiAddressMapper nsiAddressMapper;
    
    @MockBean
    public NotNsiAddressMapper notNsiAddressMapper;
    
}
