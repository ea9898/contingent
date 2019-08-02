package service.algorithms;

import moscow.ptnl.contingent.area.service.Algorithms;
import moscow.ptnl.contingent.area.service.AlgorithmsHelper;
import moscow.ptnl.contingent.area.transform.model.XMLGregorianCalendarMapper;
import moscow.ptnl.contingent.area.transform.model.esu.AreaInfoEventMapper;
import moscow.ptnl.contingent.area.transform.model.esu.AttachOnAreaChangeMapper;
import moscow.ptnl.contingent.repository.area.AddressesCRUDRepository;
import moscow.ptnl.contingent.repository.area.AreaAddressRepository;
import moscow.ptnl.contingent.repository.area.MoAddressRepository;
import moscow.ptnl.contingent.repository.nsi.AddressFormingElementCRUDRepository;
import moscow.ptnl.contingent.repository.nsi.AddressFormingElementRepository;
import moscow.ptnl.contingent.repository.nsi.BuildingRegistryCRUDRepository;
import org.junit.jupiter.api.BeforeEach;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

public class BaseTest {

    @Mock
    AddressFormingElementRepository addressFormingElementRepository;

    @Mock
    AddressFormingElementCRUDRepository addressFormingElementCRUDRepository;

    @Mock
    BuildingRegistryCRUDRepository buildingRegistryCRUDRepository;

    @Mock
    MoAddressRepository moAddressRepository;

    @Mock
    AddressesCRUDRepository addressesCRUDRepository;

    @Mock
    XMLGregorianCalendarMapper xmlGregorianCalendarMapper;

    @Mock
    AreaInfoEventMapper areaInfoEventMapper;

    @Mock
    AttachOnAreaChangeMapper attachOnAreaChangeMapper;

    @Mock
    AreaAddressRepository areaAddressRepository;

    @InjectMocks
    AlgorithmsHelper algorithmsHelper = new AlgorithmsHelper();

    @InjectMocks
    Algorithms algorithms = new Algorithms(algorithmsHelper);

    @BeforeEach
    public void init() {
        MockitoAnnotations.initMocks(this);
    }

}
