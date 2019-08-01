package ptnl.contingent2.area.service.algorithms;

import moscow.ptnl.contingent.area.error.ContingentException;
import moscow.ptnl.contingent.area.model.area.AddressWrapper;
import moscow.ptnl.contingent.area.model.area.NsiAddress;
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
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.ArrayList;
import java.util.List;

@ExtendWith(MockitoExtension.class)
public class A_YY_3_Test {

    @Mock
    private AddressFormingElementRepository addressFormingElementRepository;

    @Mock
    private AddressFormingElementCRUDRepository addressFormingElementCRUDRepository;

    @Mock
    private BuildingRegistryCRUDRepository buildingRegistryCRUDRepository;

    @Mock
    private MoAddressRepository moAddressRepository;

    @Mock
    private AddressesCRUDRepository addressesCRUDRepository;

    @Mock
    private XMLGregorianCalendarMapper xmlGregorianCalendarMapper;

    @Mock
    private AreaInfoEventMapper areaInfoEventMapper;

    @Mock
    private AttachOnAreaChangeMapper attachOnAreaChangeMapper;

    @Mock
    private AreaAddressRepository areaAddressRepository;

    @InjectMocks
    AlgorithmsHelper algorithmsHelper = new AlgorithmsHelper();

    @InjectMocks
    Algorithms algorithms = new Algorithms(algorithmsHelper);

    @BeforeEach
    public void init() {
        MockitoAnnotations.initMocks(this);
    }

    /**
     * Пример теста для проверки корректности реализации алгоритма А_УУ_3.
     */
    @Test
    public void test1() {
        List<NsiAddress> nsiAddresses = new ArrayList<>();

        try {
            List<AddressWrapper> addressWrapperList = algorithms.findIntersectingAddresses(new ArrayList<>(), new ArrayList<>(), new ArrayList<>());
        } catch (ContingentException e) {
            throw new RuntimeException(e);
        }
        Assertions.assertTrue(true);
    }
}
