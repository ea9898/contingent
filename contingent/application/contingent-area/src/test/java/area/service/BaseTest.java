package service;

import moscow.ptnl.contingent.area.transform.v1.AddressRegistryToAddressRegistryBaseMapper;
import moscow.ptnl.contingent.area.transform.v1.AddressRegistryToAddressRegistryBaseMapperImpl;
import moscow.ptnl.contingent.domain.area.Algorithms;
import moscow.ptnl.contingent.domain.area.AlgorithmsHelper;
import moscow.ptnl.contingent.area.service.HistoryServiceHelperImpl;
import moscow.ptnl.contingent.domain.area.MoMuServiceImpl;
import moscow.ptnl.contingent.domain.area.OrderServiceImpl;
import moscow.ptnl.contingent.transform.AddressesMapper;
import moscow.ptnl.contingent.transform.AreaRestrictionMapper;
import moscow.ptnl.contingent.domain.area.MoMuService;
import moscow.ptnl.contingent.domain.area.OrderService;
import moscow.ptnl.contingent.domain.area.heplers.AreaHelper;
import moscow.ptnl.contingent.nsi.domain.repository.AreaTypesRepository;
import moscow.ptnl.contingent.util.XMLGregorianCalendarMapper;
import moscow.ptnl.contingent.transform.AreaInfoEventMapper;
import moscow.ptnl.contingent.transform.AttachOnAreaChangeMapper;
import moscow.ptnl.contingent.area.configuration.EventChannelsConfiguration;
import moscow.ptnl.contingent.repository.area.AddressAllocationOrderCRUDRepository;
import moscow.ptnl.contingent.domain.area.repository.AddressAllocationOrderRepository;
import moscow.ptnl.contingent.repository.area.AddressesCRUDRepository;
import moscow.ptnl.contingent.domain.area.repository.AreaAddressRepository;
import moscow.ptnl.contingent.domain.area.repository.AreaRepository;
import moscow.ptnl.contingent.domain.area.repository.MoAddressRepository;
import moscow.ptnl.contingent.repository.area.MoAvailableAreaTypesCRUDRepository;
import moscow.ptnl.contingent.domain.area.repository.MoAvailableAreaTypesRepository;
import moscow.ptnl.contingent.repository.area.MuAvailableAreaTypesCRUDRepository;
import moscow.ptnl.contingent.domain.area.repository.MuAvailableAreaTypesRepository;
import moscow.ptnl.contingent.nsi.repository.AddressFormingElementCRUDRepository;
import moscow.ptnl.contingent.nsi.domain.repository.AddressFormingElementRepository;
import moscow.ptnl.contingent.nsi.repository.BuildingRegistryCRUDRepository;
import moscow.ptnl.contingent.service.history.HistoryServiceImpl;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.messaging.MessageChannel;

@ExtendWith(MockitoExtension.class)
public class BaseTest {

    @Mock
    public AddressFormingElementRepository addressFormingElementRepository;

    @Mock
    public AddressFormingElementCRUDRepository addressFormingElementCRUDRepository;

    @Mock
    public BuildingRegistryCRUDRepository buildingRegistryCRUDRepository;

    @Mock
    public MoAddressRepository moAddressRepository;

    @Mock
    public AddressesCRUDRepository addressesCRUDRepository;

    @Mock
    public XMLGregorianCalendarMapper xmlGregorianCalendarMapper;

    @Mock
    public AreaAddressRepository areaAddressRepository;

    @Mock
    public AreaRepository areaRepository;

    @Mock
    public MoAvailableAreaTypesRepository moAvailableAreaTypesRepository;

    @Mock
    public MoAvailableAreaTypesCRUDRepository moAvailableAreaTypesCRUDRepository;

    @Mock
    public MuAvailableAreaTypesCRUDRepository muAvailableAreaTypesCRUDRepository;

    @Mock
    public MuAvailableAreaTypesRepository muAvailableAreaTypesRepository;

    @Mock
    public AreaTypesRepository areaTypesRepository;

    @Mock
    public AddressAllocationOrderRepository addressAllocationOrderRepository;

    @Mock
    public AddressAllocationOrderCRUDRepository addressAllocationOrderCRUDRepository;

    @Mock
    @Qualifier(EventChannelsConfiguration.HISTORY_EVENT_CHANNEL_NAME)
    private MessageChannel historyChannel;

    @Spy
    public AlgorithmsHelper algorithmsHelper;

    @Spy
    public XMLGregorianCalendarMapper gregorianCalendarMapper;

    @Spy
    @InjectMocks
    public AttachOnAreaChangeMapper attachOnAreaChangeMapper;

    @InjectMocks
    public Algorithms algorithms;

    @Spy
    @InjectMocks
    public AreaInfoEventMapper areaInfoEventMapper;

    @Spy
    @InjectMocks
    public AreaRestrictionMapper areaRestrictionMapper;

    @Spy
    @InjectMocks
    private AddressesMapper addressesMapper;

    @Spy
    @InjectMocks
    private AddressRegistryToAddressRegistryBaseMapper addressRegistryToAddressRegistryBaseMapper = new AddressRegistryToAddressRegistryBaseMapperImpl();

    @Spy
    @InjectMocks
    public HistoryServiceImpl historyService;

    @Spy
    @InjectMocks
    public AreaHelper areaHelper;

    @Spy
    @InjectMocks
    private HistoryServiceHelperImpl historyServiceHelperImpl;

    @Spy
    @InjectMocks
    public MoMuService moMuService = new MoMuServiceImpl();

    @Spy
    @InjectMocks
    public OrderService orderService = new OrderServiceImpl();

    @BeforeEach
    public void init() {
        MockitoAnnotations.initMocks(this);
    }

}
