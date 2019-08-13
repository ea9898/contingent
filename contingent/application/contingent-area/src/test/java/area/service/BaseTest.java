package service;

import moscow.ptnl.contingent.area.service.Algorithms;
import moscow.ptnl.contingent.area.service.AlgorithmsHelper;
import moscow.ptnl.contingent.area.service.AreaServiceHelper;
import moscow.ptnl.contingent.area.service.AreaServiceInternal;
import moscow.ptnl.contingent.area.service.AreaServiceInternalImpl;
import moscow.ptnl.contingent.area.transform.model.XMLGregorianCalendarMapper;
import moscow.ptnl.contingent.area.transform.model.esu.AreaInfoEventMapper;
import moscow.ptnl.contingent.area.transform.model.esu.AttachOnAreaChangeMapper;
import moscow.ptnl.contingent.configuration.EventChannelsConfiguration;
import moscow.ptnl.contingent.repository.area.AddressAllocationOrderCRUDRepository;
import moscow.ptnl.contingent.repository.area.AddressAllocationOrderRepository;
import moscow.ptnl.contingent.repository.area.AddressesCRUDRepository;
import moscow.ptnl.contingent.repository.area.AreaAddressRepository;
import moscow.ptnl.contingent.repository.area.AreaRepository;
import moscow.ptnl.contingent.repository.area.MoAddressRepository;
import moscow.ptnl.contingent.repository.area.MoAvailableAreaTypesCRUDRepository;
import moscow.ptnl.contingent.repository.area.MoAvailableAreaTypesRepository;
import moscow.ptnl.contingent.repository.area.MuAvailableAreaTypesCRUDRepository;
import moscow.ptnl.contingent.repository.area.MuAvailableAreaTypesRepository;
import moscow.ptnl.contingent.repository.nsi.AddressFormingElementCRUDRepository;
import moscow.ptnl.contingent.repository.nsi.AddressFormingElementRepository;
import moscow.ptnl.contingent.repository.nsi.AreaTypesCRUDRepository;
import moscow.ptnl.contingent.repository.nsi.BuildingRegistryCRUDRepository;
import moscow.ptnl.contingent.service.history.HistoryService;
import moscow.ptnl.contingent.service.history.HistoryServiceImpl;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
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
    public AreaInfoEventMapper areaInfoEventMapper;

    @Mock
    public AreaAddressRepository areaAddressRepository;

    @Mock
    public AreaRepository areaRepository;

    @Mock
    public AreaTypesCRUDRepository areaTypesCRUDRepository;

    @Mock
    public MoAvailableAreaTypesRepository moAvailableAreaTypesRepository;

    @Mock
    public MoAvailableAreaTypesCRUDRepository moAvailableAreaTypesCRUDRepository;

    @Mock
    public MuAvailableAreaTypesCRUDRepository muAvailableAreaTypesCRUDRepository;

    @Mock
    public MuAvailableAreaTypesRepository muAvailableAreaTypesRepository;

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
    public HistoryServiceImpl historyService;

    @Spy
    @InjectMocks
    public AreaServiceHelper areaHelper;

    @InjectMocks
    public AreaServiceInternalImpl areaServiceInternal;

    @BeforeEach
    public void init() {
        MockitoAnnotations.initMocks(this);
    }

}
