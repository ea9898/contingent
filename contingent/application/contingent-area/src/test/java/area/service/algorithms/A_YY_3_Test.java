package area.service.algorithms;

import jdk.nashorn.internal.ir.annotations.Ignore;
import moscow.ptnl.contingent.area.entity.area.Addresses;
import moscow.ptnl.contingent.area.service.AlgorithmsHelper;
import moscow.ptnl.contingent.error.Validation;
import moscow.ptnl.contingent.nsi.domain.area.NsiAddressFormingElement;
import moscow.ptnl.contingent.error.ContingentException;
import moscow.ptnl.contingent.area.model.area.AddressWrapper;
import moscow.ptnl.contingent.area.model.area.NsiAddress;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import ru.mos.emias.contingent2.address.AddressRegistryBaseType;
import ru.mos.emias.contingent2.core.Address;
import service.BaseTest;

import java.util.ArrayList;
import java.util.List;

import static org.jgroups.util.Util.assertEquals;

public class A_YY_3_Test extends BaseTest {

    /**
     * Пример теста для проверки корректности реализации алгоритма А_УУ_3.
     */
    @Test
    public void search_by_global_nsi() {
        List<Addresses> addressesList = new ArrayList<>();

        Addresses addresses = new Addresses();
        addresses.setGlobalId(1000000L);
        addressesList.add(addresses);

        Addresses addresses1 = new Addresses();
        addresses1.setGlobalId(1000001L);
        addressesList.add(addresses1);

        Addresses addresses2 = new Addresses();
        addresses2.setGlobalId(1000002L);
        addressesList.add(addresses2);

        List<AddressRegistryBaseType> addressRegistryBaseTypes = new ArrayList<>();

        AddressRegistryBaseType addressRegistryBaseType1 = new AddressRegistryBaseType();
        addressRegistryBaseType1.setGlobalIdNsi(1000000L);
        addressRegistryBaseTypes.add(addressRegistryBaseType1);

        AddressRegistryBaseType addressRegistryBaseType2 = new AddressRegistryBaseType();
        addressRegistryBaseType2.setGlobalIdNsi(1000009L);
        addressRegistryBaseTypes.add(addressRegistryBaseType2);

        List<Addresses> crossAddresses =
                algorithms.findIntersectingAddressesAdd(addressRegistryBaseTypes, addressesList, new Validation());

        assertEquals(crossAddresses.size(), 1);
    }

    // 2.1 - возврат адресов
    @Test
    public void input_address_level_8_1() {
        Validation validation = new Validation();

        List<Addresses> addressesList = new ArrayList<>();

        Addresses addresses = new Addresses();
        addresses.setGlobalId(1000000L);
        addressesList.add(addresses);

        Addresses addresses1 = new Addresses();
        addresses1.setGlobalId(1000001L);
        addressesList.add(addresses1);

        Addresses addresses2 = new Addresses();
        addresses2.setGlobalId(1000002L);
        addressesList.add(addresses2);

        List<AddressRegistryBaseType> addressRegistryBaseTypes = new ArrayList<>();

        AddressRegistryBaseType addressRegistryBaseType1 = new AddressRegistryBaseType();
        addressRegistryBaseType1.setAoLevel("8");
        addressRegistryBaseTypes.add(addressRegistryBaseType1);


        List<Addresses> crossAddresses =
                algorithms.findIntersectingAddressesAdd(addressRegistryBaseTypes, addressesList, validation);

        // TODO написание тестов....
//        assertEquals(crossAddresses.size(), 1);
    }


}
