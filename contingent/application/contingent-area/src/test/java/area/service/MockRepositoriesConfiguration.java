package area.service;

import moscow.ptnl.contingent.area.transform.AddressMapper;
import moscow.ptnl.contingent.nsi.repository.AreaTypeRelationsRepository;
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
import moscow.ptnl.contingent.nsi.repository.AddressFormingElementRepository;
import moscow.ptnl.contingent.repository.area.AreaPolicyTypesCRUDRepository;
import moscow.ptnl.contingent.repository.area.AreaPolicyTypesRepository;
import moscow.ptnl.contingent.nsi.repository.AreaTypeMedicalPositionsRepository;
import moscow.ptnl.contingent.nsi.repository.AreaTypeSpecializationsRepository;
import moscow.ptnl.contingent.nsi.repository.AreaTypesCRUDRepository;
import moscow.ptnl.contingent.nsi.repository.BuildingRegistryRepository;
import moscow.ptnl.contingent.nsi.repository.PolicyTypeRepository;
import moscow.ptnl.contingent.nsi.repository.PositionCodeRepository;
import moscow.ptnl.contingent.nsi.repository.PositionNomRepository;

import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Configuration;

/**
 *
 * @author sorlov
 */
@Configuration
public class MockRepositoriesConfiguration {

    @MockBean
    public MoAddressRepository moAddressRepository;
    
    @MockBean
    public AddressFormingElementRepository addressFormingElementRepository;

    @MockBean
    public AreaAddressRepository areaAddressRepository;

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

    @MockBean
    public MuAvailableAreaTypesCRUDRepository muAvailableAreaTypesCRUDRepository;
    
    @MockBean
    public MuAvailableAreaTypesRepository muAvailableAreaTypesRepository;

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
    private PositionCodeRepository positionCodeRepository;

    @MockBean
    private AreaTypeRelationsRepository areaTypeRelationsRepository;

}
