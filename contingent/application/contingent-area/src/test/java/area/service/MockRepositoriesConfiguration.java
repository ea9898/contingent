package area.service;

import moscow.ptnl.contingent.nsi.domain.repository.AreaTypeProfileRepository;
import moscow.ptnl.contingent.nsi.domain.repository.AreaTypeRelationsRepository;
import moscow.ptnl.contingent.nsi.domain.repository.AreaTypesRepository;
import moscow.ptnl.contingent.nsi.domain.repository.SpecializationRepository;
import moscow.ptnl.contingent.nsi.repository.SpecializationCRUDRepository;
import moscow.ptnl.contingent.repository.SysopCRUDRepository;
import moscow.ptnl.contingent.repository.SysopMsgCRUDRepository;
import moscow.ptnl.contingent.repository.SysopMsgParamCRUDRepository;
import moscow.ptnl.contingent.repository.area.AddressAllocationOrderCRUDRepository;
import moscow.ptnl.contingent.domain.area.repository.AddressAllocationOrderRepository;
import moscow.ptnl.contingent.repository.area.AddressesCRUDRepository;
import moscow.ptnl.contingent.domain.area.repository.AddressesRepository;
import moscow.ptnl.contingent.repository.area.AreaAddressPagingAndSortingRepository;
import moscow.ptnl.contingent.domain.area.repository.AreaAddressRepository;
import moscow.ptnl.contingent.repository.area.AreaCRUDRepository;
import moscow.ptnl.contingent.repository.area.AreaMedicalEmployeeCRUDRepository;
import moscow.ptnl.contingent.domain.area.repository.AreaMedicalEmployeeRepository;
import moscow.ptnl.contingent.domain.area.repository.AreaRepository;
import moscow.ptnl.contingent.repository.area.AreaToAreaTypeCRUDRepository;
import moscow.ptnl.contingent.domain.area.repository.AreaToAreaTypeRepository;
import moscow.ptnl.contingent.repository.area.MoAddressCRUDRepository;
import moscow.ptnl.contingent.domain.area.repository.MoAddressRepository;
import moscow.ptnl.contingent.repository.area.MoAvailableAreaTypesCRUDRepository;
import moscow.ptnl.contingent.domain.area.repository.MoAvailableAreaTypesRepository;
import moscow.ptnl.contingent.repository.area.MuAddlAreaTypesRepository;
import moscow.ptnl.contingent.repository.area.MuAvailableAreaTypesCRUDRepository;
import moscow.ptnl.contingent.domain.area.repository.MuAvailableAreaTypesRepository;
import moscow.ptnl.contingent.nsi.domain.repository.AddressFormingElementRepository;
import moscow.ptnl.contingent.repository.area.AreaPolicyTypesCRUDRepository;
import moscow.ptnl.contingent.domain.area.repository.AreaPolicyTypesRepository;
import moscow.ptnl.contingent.nsi.domain.repository.AreaTypeMedicalPositionsRepository;
import moscow.ptnl.contingent.nsi.domain.repository.AreaTypeSpecializationsRepository;
import moscow.ptnl.contingent.nsi.repository.AreaTypesCRUDRepository;
import moscow.ptnl.contingent.nsi.repository.BuildingRegistryRepository;
import moscow.ptnl.contingent.nsi.domain.repository.PolicyTypeRepository;
import moscow.ptnl.contingent.nsi.domain.repository.PositionCodeRepository;
import moscow.ptnl.contingent.nsi.domain.repository.PositionNomRepository;

import moscow.ptnl.contingent.sysop.repository.SysopRepository;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Configuration;

import javax.persistence.EntityManager;

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
    public AreaAddressPagingAndSortingRepository areaAddressPagingAndSortingRepository;
    
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
    public AreaTypesRepository areaTypesRepository;

    @MockBean
    private PositionCodeRepository positionCodeRepository;

    @MockBean
    private AreaTypeRelationsRepository areaTypeRelationsRepository;

    @MockBean
    private SysopCRUDRepository sysopCRUDRepository;

    @MockBean
    private SysopMsgCRUDRepository sysopMsgCRUDRepository;

    @MockBean
    private SysopRepository sysopRepository;

    @MockBean
    private SysopMsgParamCRUDRepository sysopMsgParamCRUDRepository;

    @MockBean
    private SpecializationRepository specializationRepository;

    @MockBean
    private AreaTypeProfileRepository areaTypeProfileRepository;

    @MockBean
    private EntityManager entityManager;
}
