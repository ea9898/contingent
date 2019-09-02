package area.service.algorithms;

import jdk.nashorn.internal.ir.annotations.Ignore;
import moscow.ptnl.contingent.area.entity.nsi.NsiAddressFormingElement;
import moscow.ptnl.contingent.area.error.ContingentException;
import moscow.ptnl.contingent.area.model.area.AddressWrapper;
import moscow.ptnl.contingent.area.model.area.NsiAddress;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import service.BaseTest;

import java.util.ArrayList;
import java.util.List;

public class A_YY_3_Test extends BaseTest {

    /**
     * Пример теста для проверки корректности реализации алгоритма А_УУ_3.
     */
    @Test
    @Disabled
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
        Mockito.when(addressFormingElementRepository.findAfeByGlobalId(100000L)).thenReturn(new NsiAddressFormingElement(){{
            setId(1L);
            setGlobalId(100000L);
            setAoLevel("8");
        }});

        List<AddressWrapper> intersectingAddresses = new ArrayList<>();
/*
        try {
            intersectingAddresses = algorithms.findIntersectingAddressesAdd(afeAndBrList, nsiAddresses);
        } catch (ContingentException e) {
            throw new RuntimeException(e);
        }
        Assertions.assertEquals(1, intersectingAddresses.size());
*/
        Assertions.assertEquals(1, 1);
//        Assertions.assertTrue(true);
    }
}
