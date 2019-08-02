package service.algorithms;

import jdk.nashorn.internal.ir.annotations.Ignore;
import moscow.ptnl.contingent.area.entity.nsi.AddressFormingElement;
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
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.ArrayList;
import java.util.List;

@ExtendWith(MockitoExtension.class)
public class A_YY_3_Test extends BaseTest {

    /**
     * Пример теста для проверки корректности реализации алгоритма А_УУ_3.
     */
    @Test
    public void test1() {

        // Формирование NSIAddress
        List<NsiAddress> nsiAddresses = new ArrayList<>();
        NsiAddress nsiAddress1 = new NsiAddress(100000L, 8);
        nsiAddresses.add(nsiAddress1);

        // Формирование AFE + BR
        List<AddressWrapper> afeAndBrList = new ArrayList<>();
        AddressWrapper addressWrapper1 = new AddressWrapper();
        addressWrapper1.setBrGlobalId(100000L);
        afeAndBrList.add(addressWrapper1);

        // Формирование в БД значений AFE
        Mockito.when(addressFormingElementRepository.findAfeByGlobalId(100000L)).thenReturn(new AddressFormingElement(){{
            setId(1L);
            setGlobalId(100000L);
            setAoLevel("8");
        }});

        List<AddressWrapper> intersectingAddresses = new ArrayList<>();
        try {
            intersectingAddresses = algorithms.findIntersectingAddresses(afeAndBrList, nsiAddresses);
        } catch (ContingentException e) {
            throw new RuntimeException(e);
        }
        Assertions.assertEquals(1, intersectingAddresses.size());
//        Assertions.assertTrue(true);
    }
}
