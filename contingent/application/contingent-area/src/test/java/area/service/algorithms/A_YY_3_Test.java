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
import ru.mos.emias.contingent2.address.Area;
import ru.mos.emias.contingent2.address.AreaOMKTE;
import ru.mos.emias.contingent2.address.Place;
import ru.mos.emias.contingent2.address.Plan;
import ru.mos.emias.contingent2.address.RegionOMKTE;
import ru.mos.emias.contingent2.address.Street;
import ru.mos.emias.contingent2.core.Address;
import service.BaseTest;

import java.util.ArrayList;
import java.util.List;

import static org.jgroups.util.Util.assertEquals;
import static org.jgroups.util.Util.assertFalse;

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

    // 2 -> b.1 -> 2.1 - возврат адресов
    // Поиск по переданным STREETCODE и AOLEVEL=7
    @Test
    public void input_address_level_8_1() {
        Validation validation = new Validation();

        List<Addresses> addressesList = new ArrayList<>();

        Addresses addresses = new Addresses();
        addresses.setGlobalId(1000000L);
        addresses.setAoLevel("7");
        addresses.setStreetCode("STREET_CODE_1");
        addressesList.add(addresses);

        Addresses addresses1 = new Addresses();
        addresses1.setGlobalId(1000001L);
        addresses1.setAoLevel("7");
        addresses1.setStreetCode("STREET_CODE_1");
        addressesList.add(addresses1);

        Addresses addresses2 = new Addresses();
        addresses2.setGlobalId(1000002L);
        addresses2.setAoLevel("3");
        addressesList.add(addresses2);

        List<AddressRegistryBaseType> addressRegistryBaseTypes = new ArrayList<>();

        AddressRegistryBaseType addressRegistryBaseType1 = new AddressRegistryBaseType();
        addressRegistryBaseType1.setAoLevel("8");
        Street street = new Street();
        street.setCode("STREET_CODE_1");
        addressRegistryBaseType1.setStreet(street);
        addressRegistryBaseTypes.add(addressRegistryBaseType1);

        List<Addresses> crossAddresses =
                algorithms.findIntersectingAddressesAdd(addressRegistryBaseTypes, addressesList, validation);

        assertEquals(crossAddresses.size(), 2);
    }

    // 2 -> b.1 -> 2.1 - возврат адресов
    // Поиск по переданным PLANCODE, STREETCODE и AOLEVEL=7
    @Test
    public void input_address_level_8_2() {
        Validation validation = new Validation();

        List<Addresses> addressesList = new ArrayList<>();

        Addresses addresses = new Addresses();
        addresses.setGlobalId(1000000L);
        addresses.setAoLevel("7");
        addresses.setStreetCode("STREET_CODE_1");
        addressesList.add(addresses);

        Addresses addresses1 = new Addresses();
        addresses1.setGlobalId(1000001L);
        addresses1.setAoLevel("7");
        addresses1.setStreetCode("STREET_CODE_1");
        addresses1.setPlanCode("PLAN_CODE_1");
        addressesList.add(addresses1);

        Addresses addresses2 = new Addresses();
        addresses2.setGlobalId(1000002L);
        addresses2.setAoLevel("3");
        addressesList.add(addresses2);

        Addresses addresses3 = new Addresses();
        addresses3.setGlobalId(1000003L);
        addresses3.setAoLevel("7");
        addresses3.setStreetCode("STREET_CODE_1");
        addresses3.setPlanCode("PLAN_CODE_1");
        addressesList.add(addresses3);

        List<AddressRegistryBaseType> addressRegistryBaseTypes = new ArrayList<>();

        AddressRegistryBaseType addressRegistryBaseType1 = new AddressRegistryBaseType();
        addressRegistryBaseType1.setAoLevel("8");
        Street street = new Street();
        street.setCode("STREET_CODE_1");
        addressRegistryBaseType1.setStreet(street);
        addressRegistryBaseTypes.add(addressRegistryBaseType1);

        List<Addresses> crossAddresses =
                algorithms.findIntersectingAddressesAdd(addressRegistryBaseTypes, addressesList, validation);

        assertEquals(crossAddresses.size(), 3);
    }

    // 2 -> b.1 -> 2.1 - возврат адресов
    // Поиск по переданным PLANCODE, STREETCODE и AOLEVEL=7
    @Test
    public void input_address_level_8_3() {
        Validation validation = new Validation();

        List<Addresses> addressesList = new ArrayList<>();

        Addresses addresses = new Addresses();
        addresses.setGlobalId(1000000L);
        addresses.setAoLevel("7");
        addresses.setStreetCode("STREET_CODE_1");
        addressesList.add(addresses);

        Addresses addresses1 = new Addresses();
        addresses1.setGlobalId(1000001L);
        addresses1.setAoLevel("7");
        addresses1.setStreetCode("STREET_CODE_1");
        addresses1.setPlanCode("PLAN_CODE_1");
        addressesList.add(addresses1);

        Addresses addresses2 = new Addresses();
        addresses2.setGlobalId(1000002L);
        addresses2.setAoLevel("3");
        addressesList.add(addresses2);

        Addresses addresses3 = new Addresses();
        addresses3.setGlobalId(1000003L);
        addresses3.setAoLevel("7");
        addresses3.setStreetCode("STREET_CODE_1");
        addresses3.setPlanCode("PLAN_CODE_1");
        addressesList.add(addresses3);

        List<AddressRegistryBaseType> addressRegistryBaseTypes = new ArrayList<>();

        AddressRegistryBaseType addressRegistryBaseType1 = new AddressRegistryBaseType();
        addressRegistryBaseType1.setAoLevel("8");
        Street street = new Street();
        street.setCode("STREET_CODE_1");
        addressRegistryBaseType1.setStreet(street);
        Plan plan = new Plan();
        plan.setCode("PLAN_CODE_1");
        addressRegistryBaseType1.setPlan(plan);
        addressRegistryBaseTypes.add(addressRegistryBaseType1);

        List<Addresses> crossAddresses =
                algorithms.findIntersectingAddressesAdd(addressRegistryBaseTypes, addressesList, validation);

        assertEquals(crossAddresses.size(), 2);
    }


    // 2 -> b.1 -> 2.1 - возврат адресов
    // Поиск по переданным PLACECODE, PLANCODE, STREETCODE и AOLEVEL=7
    @Test
    public void input_address_level_8_4() {
        Validation validation = new Validation();

        List<Addresses> addressesList = new ArrayList<>();

        Addresses addresses = new Addresses();
        addresses.setGlobalId(1000000L);
        addresses.setAoLevel("7");
        addresses.setStreetCode("STREET_CODE_1");
        addressesList.add(addresses);

        Addresses addresses1 = new Addresses();
        addresses1.setGlobalId(1000001L);
        addresses1.setAoLevel("7");
        addresses1.setStreetCode("STREET_CODE_1");
        addresses1.setPlanCode("PLAN_CODE_1");
        addresses1.setPlaceCode("PLACECODE_CODE_1");
        addressesList.add(addresses1);

        Addresses addresses2 = new Addresses();
        addresses2.setGlobalId(1000002L);
        addresses2.setAoLevel("3");
        addressesList.add(addresses2);

        Addresses addresses3 = new Addresses();
        addresses3.setGlobalId(1000003L);
        addresses3.setAoLevel("7");
        addresses3.setStreetCode("STREET_CODE_1");
        addresses3.setPlanCode("PLAN_CODE_1");
        addressesList.add(addresses3);

        List<AddressRegistryBaseType> addressRegistryBaseTypes = new ArrayList<>();

        AddressRegistryBaseType addressRegistryBaseType1 = new AddressRegistryBaseType();
        addressRegistryBaseType1.setAoLevel("8");
        Street street = new Street();
        street.setCode("STREET_CODE_1");
        addressRegistryBaseType1.setStreet(street);
        Plan plan = new Plan();
        plan.setCode("PLAN_CODE_1");
        addressRegistryBaseType1.setPlan(plan);
        Place place = new Place();
        place.setCode("PLACECODE_CODE_1");
        addressRegistryBaseType1.setPlace(place);
        addressRegistryBaseTypes.add(addressRegistryBaseType1);

        List<Addresses> crossAddresses =
                algorithms.findIntersectingAddressesAdd(addressRegistryBaseTypes, addressesList, validation);

        assertEquals(crossAddresses.size(), 1);
    }


    // 2 -> b.1 -> 2.1 - возврат ошибки
    // Поиск по переданным AREACODE, PLACECODE, PLANCODE, STREETCODE и AOLEVEL=7
    @Test
    public void input_address_level_8_5() {
        Validation validation = new Validation();

        List<Addresses> addressesList = new ArrayList<>();

        Addresses addresses = new Addresses();
        addresses.setGlobalId(1000000L);
        addresses.setAoLevel("7");
        addresses.setStreetCode("STREET_CODE_1");
        addressesList.add(addresses);

        Addresses addresses1 = new Addresses();
        addresses1.setGlobalId(1000001L);
        addresses1.setAoLevel("7");
        addresses1.setStreetCode("STREET_CODE_1");
        addresses1.setPlanCode("PLAN_CODE_1");
        addresses1.setPlaceCode("PLACECODE_CODE_1");
        addressesList.add(addresses1);

        Addresses addresses2 = new Addresses();
        addresses2.setGlobalId(1000002L);
        addresses2.setAoLevel("3");
        addressesList.add(addresses2);

        Addresses addresses3 = new Addresses();
        addresses3.setGlobalId(1000003L);
        addresses3.setAoLevel("7");
        addresses3.setStreetCode("STREET_CODE_1");
        addresses3.setPlanCode("PLAN_CODE_1");
        addressesList.add(addresses3);

        List<AddressRegistryBaseType> addressRegistryBaseTypes = new ArrayList<>();

        AddressRegistryBaseType addressRegistryBaseType1 = new AddressRegistryBaseType();
        addressRegistryBaseType1.setAoLevel("8");
        Street street = new Street();
        street.setCode("STREET_CODE_1");
        addressRegistryBaseType1.setStreet(street);
        Plan plan = new Plan();
        plan.setCode("PLAN_CODE_1");
        addressRegistryBaseType1.setPlan(plan);
        Place place = new Place();
        place.setCode("PLACECODE_CODE_1");
        addressRegistryBaseType1.setPlace(place);
        Area area = new Area();
        area.setCode("AREA_CODE_1");
        addressRegistryBaseType1.setArea(area);
        addressRegistryBaseTypes.add(addressRegistryBaseType1);

        List<Addresses> crossAddresses =
                algorithms.findIntersectingAddressesAdd(addressRegistryBaseTypes, addressesList, validation);

        assertFalse(validation.isSuccess());
        assertEquals("UE070", validation.getMessages().get(0).getCode());
    }

    // 2 -> b.1 -> 2.1 - возврат адресов
    // Поиск по переданным AREACODE, PLACECODE, PLANCODE, STREETCODE и AOLEVEL=7
    @Test
    public void input_address_level_8_6() {
        Validation validation = new Validation();

        List<Addresses> addressesList = new ArrayList<>();

        Addresses addresses = new Addresses();
        addresses.setGlobalId(1000000L);
        addresses.setAoLevel("7");
        addresses.setStreetCode("STREET_CODE_1");
        addressesList.add(addresses);

        Addresses addresses1 = new Addresses();
        addresses1.setGlobalId(1000001L);
        addresses1.setAoLevel("7");
        addresses1.setStreetCode("STREET_CODE_1");
        addresses1.setPlanCode("PLAN_CODE_1");
        addresses1.setPlaceCode("PLACECODE_CODE_1");
        addressesList.add(addresses1);

        Addresses addresses2 = new Addresses();
        addresses2.setGlobalId(1000002L);
        addresses2.setAoLevel("3");
        addressesList.add(addresses2);

        Addresses addresses3 = new Addresses();
        addresses3.setGlobalId(1000003L);
        addresses3.setAoLevel("7");
        addresses3.setStreetCode("STREET_CODE_1");
        addresses3.setPlanCode("PLAN_CODE_1");
        addresses3.setPlaceCode("PLACECODE_CODE_1");
        addresses3.setAreaCode("AREA_CODE_1");
        addressesList.add(addresses3);

        List<AddressRegistryBaseType> addressRegistryBaseTypes = new ArrayList<>();

        AddressRegistryBaseType addressRegistryBaseType1 = new AddressRegistryBaseType();
        addressRegistryBaseType1.setAoLevel("8");
        Street street = new Street();
        street.setCode("STREET_CODE_1");
        addressRegistryBaseType1.setStreet(street);
        Plan plan = new Plan();
        plan.setCode("PLAN_CODE_1");
        addressRegistryBaseType1.setPlan(plan);
        Place place = new Place();
        place.setCode("PLACECODE_CODE_1");
        addressRegistryBaseType1.setPlace(place);
        Area area = new Area();
        area.setCode("AREA_CODE_1");
        addressRegistryBaseType1.setArea(area);
        addressRegistryBaseTypes.add(addressRegistryBaseType1);

        List<Addresses> crossAddresses =
                algorithms.findIntersectingAddressesAdd(addressRegistryBaseTypes, addressesList, validation);

        assertEquals(crossAddresses.size(), 1);
        assertEquals(crossAddresses.get(0).getGlobalId(), 1000003L);
    }

    // 2 -> b.1 -> 2.1 - нет возврата адресов
    // Поиск по переданным AREACODE_OMK_TE(1 код передан), AREACODE, PLACECODE, PLANCODE, STREETCODE и AOLEVEL=7
    @Test
    public void input_address_level_8_7() {
        Validation validation = new Validation();

        List<Addresses> addressesList = new ArrayList<>();

        Addresses addresses = new Addresses();
        addresses.setGlobalId(1000000L);
        addresses.setAoLevel("7");
        addresses.setStreetCode("STREET_CODE_1");
        addressesList.add(addresses);

        Addresses addresses1 = new Addresses();
        addresses1.setGlobalId(1000001L);
        addresses1.setAoLevel("7");
        addresses1.setStreetCode("STREET_CODE_1");
        addresses1.setPlanCode("PLAN_CODE_1");
        addresses1.setPlaceCode("PLACECODE_CODE_1");
        addressesList.add(addresses1);

        Addresses addresses2 = new Addresses();
        addresses2.setGlobalId(1000002L);
        addresses2.setAoLevel("3");
        addressesList.add(addresses2);

        Addresses addresses3 = new Addresses();
        addresses3.setGlobalId(1000003L);
        addresses3.setAoLevel("7");
        addresses3.setStreetCode("STREET_CODE_1");
        addresses3.setPlanCode("PLAN_CODE_1");
        addresses3.setPlaceCode("PLACECODE_CODE_1");
        addresses3.setAreaCode("AREA_CODE_1");
        addressesList.add(addresses3);

        List<AddressRegistryBaseType> addressRegistryBaseTypes = new ArrayList<>();

        AddressRegistryBaseType addressRegistryBaseType1 = new AddressRegistryBaseType();
        addressRegistryBaseType1.setAoLevel("8");
        Street street = new Street();
        street.setCode("STREET_CODE_1");
        addressRegistryBaseType1.setStreet(street);
        Plan plan = new Plan();
        plan.setCode("PLAN_CODE_1");
        addressRegistryBaseType1.setPlan(plan);
        Place place = new Place();
        place.setCode("PLACECODE_CODE_1");
        addressRegistryBaseType1.setPlace(place);
        Area area = new Area();
        area.setCode("AREA_CODE_1");
        addressRegistryBaseType1.setArea(area);
        AreaOMKTE areaOMKTE = new AreaOMKTE();
        areaOMKTE.setCode("AREA_OMKTE_CODE_1");
        addressRegistryBaseType1.setAreaOMKTE(areaOMKTE);
        addressRegistryBaseTypes.add(addressRegistryBaseType1);

        List<Addresses> crossAddresses =
                algorithms.findIntersectingAddressesAdd(addressRegistryBaseTypes, addressesList, validation);

        assertEquals(crossAddresses.size(), 0);
    }

    // 2 -> b.1 -> 2.1 - возврат 1 адреса
    // Поиск по переданным AREACODE_OMK_TE(1 код передан), AREACODE, PLACECODE, PLANCODE, STREETCODE и AOLEVEL=7
    @Test
    public void input_address_level_8_8() {
        Validation validation = new Validation();

        List<Addresses> addressesList = new ArrayList<>();

        Addresses addresses = new Addresses();
        addresses.setGlobalId(1000000L);
        addresses.setAoLevel("7");
        addresses.setStreetCode("STREET_CODE_1");
        addressesList.add(addresses);

        Addresses addresses1 = new Addresses();
        addresses1.setGlobalId(1000001L);
        addresses1.setAoLevel("7");
        addresses1.setStreetCode("STREET_CODE_1");
        addresses1.setPlanCode("PLAN_CODE_1");
        addresses1.setPlaceCode("PLACECODE_CODE_1");
        addresses1.setAreaCode("AREA_CODE_1");
        addresses1.setAreaCodeOmkTe("AREA_OMKTE_CODE_2");
        addressesList.add(addresses1);

        Addresses addresses2 = new Addresses();
        addresses2.setGlobalId(1000002L);
        addresses2.setAoLevel("3");
        addressesList.add(addresses2);

        Addresses addresses3 = new Addresses();
        addresses3.setGlobalId(1000003L);
        addresses3.setAoLevel("7");
        addresses3.setStreetCode("STREET_CODE_1");
        addresses3.setPlanCode("PLAN_CODE_1");
        addresses3.setPlaceCode("PLACECODE_CODE_1");
        addresses3.setAreaCode("AREA_CODE_1");
        addresses3.setAreaCodeOmkTe("AREA_OMKTE_CODE_1");
        addressesList.add(addresses3);

        List<AddressRegistryBaseType> addressRegistryBaseTypes = new ArrayList<>();

        AddressRegistryBaseType addressRegistryBaseType1 = new AddressRegistryBaseType();
        addressRegistryBaseType1.setAoLevel("8");
        Street street = new Street();
        street.setCode("STREET_CODE_1");
        addressRegistryBaseType1.setStreet(street);
        Plan plan = new Plan();
        plan.setCode("PLAN_CODE_1");
        addressRegistryBaseType1.setPlan(plan);
        Place place = new Place();
        place.setCode("PLACECODE_CODE_1");
        addressRegistryBaseType1.setPlace(place);
        Area area = new Area();
        area.setCode("AREA_CODE_1");
        addressRegistryBaseType1.setArea(area);
        AreaOMKTE areaOMKTE = new AreaOMKTE();
        areaOMKTE.setCode("AREA_OMKTE_CODE_1");
        addressRegistryBaseType1.setAreaOMKTE(areaOMKTE);
        addressRegistryBaseTypes.add(addressRegistryBaseType1);

        List<Addresses> crossAddresses =
                algorithms.findIntersectingAddressesAdd(addressRegistryBaseTypes, addressesList, validation);

        assertEquals(crossAddresses.size(), 1);
        assertEquals(crossAddresses.get(0).getGlobalId(), 1000003L);
    }


    // 2 -> b.1 -> 2.1 - возврат 1 адреса
    // Поиск по переданным AREACODE_OMK_TE(2 кода переданы), AREACODE, PLACECODE, PLANCODE, STREETCODE и AOLEVEL=7
    @Test
    public void input_address_level_8_9() {
        Validation validation = new Validation();

        List<Addresses> addressesList = new ArrayList<>();

        Addresses addresses = new Addresses();
        addresses.setGlobalId(1000000L);
        addresses.setAoLevel("7");
        addresses.setStreetCode("STREET_CODE_1");
        addressesList.add(addresses);

        Addresses addresses1 = new Addresses();
        addresses1.setGlobalId(1000001L);
        addresses1.setAoLevel("7");
        addresses1.setStreetCode("STREET_CODE_1");
        addresses1.setPlanCode("PLAN_CODE_1");
        addresses1.setPlaceCode("PLACECODE_CODE_1");
        addresses1.setAreaCode("AREA_CODE_1");
        addressesList.add(addresses1);

        Addresses addresses2 = new Addresses();
        addresses2.setGlobalId(1000002L);
        addresses2.setAoLevel("3");
        addressesList.add(addresses2);

        Addresses addresses3 = new Addresses();
        addresses3.setGlobalId(1000003L);
        addresses3.setAoLevel("7");
        addresses3.setStreetCode("STREET_CODE_1");
        addresses3.setPlanCode("PLAN_CODE_1");
        addresses3.setPlaceCode("PLACECODE_CODE_1");
        addresses3.setAreaCode("AREA_CODE_1");
        addresses3.setAreaCodeOmkTe("AREA_OMKTE_CODE_1");
        addressesList.add(addresses3);

        List<AddressRegistryBaseType> addressRegistryBaseTypes = new ArrayList<>();

        AddressRegistryBaseType addressRegistryBaseType1 = new AddressRegistryBaseType();
        addressRegistryBaseType1.setAoLevel("8");
        Street street = new Street();
        street.setCode("STREET_CODE_1");
        addressRegistryBaseType1.setStreet(street);
        Plan plan = new Plan();
        plan.setCode("PLAN_CODE_1");
        addressRegistryBaseType1.setPlan(plan);
        Place place = new Place();
        place.setCode("PLACECODE_CODE_1");
        addressRegistryBaseType1.setPlace(place);
        Area area = new Area();
        area.setCode("AREA_CODE_1");
        addressRegistryBaseType1.setArea(area);
        AreaOMKTE areaOMKTE = new AreaOMKTE();
        areaOMKTE.setCode("AREA_OMKTE_CODE_1;AREA_OMKTE_CODE_2");
        addressRegistryBaseType1.setAreaOMKTE(areaOMKTE);
        addressRegistryBaseTypes.add(addressRegistryBaseType1);

        List<Addresses> crossAddresses =
                algorithms.findIntersectingAddressesAdd(addressRegistryBaseTypes, addressesList, validation);

        assertEquals(crossAddresses.size(), 1);
        assertEquals(crossAddresses.get(0).getGlobalId(), 1000003L);
    }


    // 2 -> b.1 -> 2.1 - возврат 2-х адресов
    // Поиск по переданным AREACODE_OMK_TE(2 кода переданы), AREACODE, PLACECODE, PLANCODE, STREETCODE и AOLEVEL=7
    @Test
    public void input_address_level_8_10() {
        Validation validation = new Validation();

        List<Addresses> addressesList = new ArrayList<>();

        Addresses addresses = new Addresses();
        addresses.setGlobalId(1000000L);
        addresses.setAoLevel("7");
        addresses.setStreetCode("STREET_CODE_1");
        addressesList.add(addresses);

        Addresses addresses1 = new Addresses();
        addresses1.setGlobalId(1000001L);
        addresses1.setAoLevel("7");
        addresses1.setStreetCode("STREET_CODE_1");
        addresses1.setPlanCode("PLAN_CODE_1");
        addresses1.setPlaceCode("PLACECODE_CODE_1");
        addresses1.setAreaCode("AREA_CODE_1");
        addresses1.setAreaCodeOmkTe("AREA_OMKTE_CODE_2");
        addressesList.add(addresses1);

        Addresses addresses2 = new Addresses();
        addresses2.setGlobalId(1000002L);
        addresses2.setAoLevel("3");
        addressesList.add(addresses2);

        Addresses addresses3 = new Addresses();
        addresses3.setGlobalId(1000003L);
        addresses3.setAoLevel("7");
        addresses3.setStreetCode("STREET_CODE_1");
        addresses3.setPlanCode("PLAN_CODE_1");
        addresses3.setPlaceCode("PLACECODE_CODE_1");
        addresses3.setAreaCode("AREA_CODE_1");
        addresses3.setAreaCodeOmkTe("AREA_OMKTE_CODE_1");
        addressesList.add(addresses3);

        List<AddressRegistryBaseType> addressRegistryBaseTypes = new ArrayList<>();

        AddressRegistryBaseType addressRegistryBaseType1 = new AddressRegistryBaseType();
        addressRegistryBaseType1.setAoLevel("8");
        Street street = new Street();
        street.setCode("STREET_CODE_1");
        addressRegistryBaseType1.setStreet(street);
        Plan plan = new Plan();
        plan.setCode("PLAN_CODE_1");
        addressRegistryBaseType1.setPlan(plan);
        Place place = new Place();
        place.setCode("PLACECODE_CODE_1");
        addressRegistryBaseType1.setPlace(place);
        Area area = new Area();
        area.setCode("AREA_CODE_1");
        addressRegistryBaseType1.setArea(area);
        AreaOMKTE areaOMKTE = new AreaOMKTE();
        areaOMKTE.setCode("AREA_OMKTE_CODE_1;AREA_OMKTE_CODE_2");
        addressRegistryBaseType1.setAreaOMKTE(areaOMKTE);
        addressRegistryBaseTypes.add(addressRegistryBaseType1);

        List<Addresses> crossAddresses =
                algorithms.findIntersectingAddressesAdd(addressRegistryBaseTypes, addressesList, validation);

        assertEquals(crossAddresses.size(), 2);
    }

    // 2 -> b.1 -> 2.1 - нет возвращаемых адресов
    // Поиск по переданным REGION_TE_CODE(1 код), AREACODE_OMK_TE(2 кода переданы), AREACODE, PLACECODE, PLANCODE, STREETCODE и AOLEVEL=7
    @Test
    public void input_address_level_8_11() {
        Validation validation = new Validation();

        List<Addresses> addressesList = new ArrayList<>();

        Addresses addresses = new Addresses();
        addresses.setGlobalId(1000000L);
        addresses.setAoLevel("7");
        addresses.setStreetCode("STREET_CODE_1");
        addressesList.add(addresses);

        Addresses addresses1 = new Addresses();
        addresses1.setGlobalId(1000001L);
        addresses1.setAoLevel("7");
        addresses1.setStreetCode("STREET_CODE_1");
        addresses1.setPlanCode("PLAN_CODE_1");
        addresses1.setPlaceCode("PLACECODE_CODE_1");
        addresses1.setAreaCode("AREA_CODE_1");
        addresses1.setAreaCodeOmkTe("AREA_OMKTE_CODE_2");
        addressesList.add(addresses1);

        Addresses addresses2 = new Addresses();
        addresses2.setGlobalId(1000002L);
        addresses2.setAoLevel("3");
        addressesList.add(addresses2);

        Addresses addresses3 = new Addresses();
        addresses3.setGlobalId(1000003L);
        addresses3.setAoLevel("7");
        addresses3.setStreetCode("STREET_CODE_1");
        addresses3.setPlanCode("PLAN_CODE_1");
        addresses3.setPlaceCode("PLACECODE_CODE_1");
        addresses3.setAreaCode("AREA_CODE_1");
        addresses3.setAreaCodeOmkTe("AREA_OMKTE_CODE_1");
        addressesList.add(addresses3);

        List<AddressRegistryBaseType> addressRegistryBaseTypes = new ArrayList<>();

        AddressRegistryBaseType addressRegistryBaseType1 = new AddressRegistryBaseType();
        addressRegistryBaseType1.setAoLevel("8");
        Street street = new Street();
        street.setCode("STREET_CODE_1");
        addressRegistryBaseType1.setStreet(street);
        Plan plan = new Plan();
        plan.setCode("PLAN_CODE_1");
        addressRegistryBaseType1.setPlan(plan);
        Place place = new Place();
        place.setCode("PLACECODE_CODE_1");
        addressRegistryBaseType1.setPlace(place);
        Area area = new Area();
        area.setCode("AREA_CODE_1");
        addressRegistryBaseType1.setArea(area);
        AreaOMKTE areaOMKTE = new AreaOMKTE();
        areaOMKTE.setCode("AREA_OMKTE_CODE_1;AREA_OMKTE_CODE_2");
        addressRegistryBaseType1.setAreaOMKTE(areaOMKTE);
        RegionOMKTE regionOMKTE = new RegionOMKTE();
        regionOMKTE.setCode("REGION_OMKTE_CODE_1");
        addressRegistryBaseType1.setRegionOMKTE(regionOMKTE);
        addressRegistryBaseTypes.add(addressRegistryBaseType1);

        List<Addresses> crossAddresses =
                algorithms.findIntersectingAddressesAdd(addressRegistryBaseTypes, addressesList, validation);

        assertEquals(crossAddresses.size(), 0);
    }

    // 2 -> b.1 -> 2.1 - возврат 1 адреса
    // Поиск по переданным REGION_TE_CODE(3 кода), AREACODE_OMK_TE(2 кода переданы), AREACODE, PLACECODE, PLANCODE, STREETCODE и AOLEVEL=7
    @Test
    public void input_address_level_8_12() {
        Validation validation = new Validation();

        List<Addresses> addressesList = new ArrayList<>();

        Addresses addresses = new Addresses();
        addresses.setGlobalId(1000000L);
        addresses.setAoLevel("7");
        addresses.setStreetCode("STREET_CODE_1");
        addressesList.add(addresses);

        Addresses addresses1 = new Addresses();
        addresses1.setGlobalId(1000001L);
        addresses1.setAoLevel("7");
        addresses1.setStreetCode("STREET_CODE_1");
        addresses1.setPlanCode("PLAN_CODE_1");
        addresses1.setPlaceCode("PLACECODE_CODE_1");
        addresses1.setAreaCode("AREA_CODE_1");
        addresses1.setAreaCodeOmkTe("AREA_OMKTE_CODE_2");
        addresses1.setRegionTeCode("REGION_OMKTE_CODE_1");
        addressesList.add(addresses1);

        Addresses addresses2 = new Addresses();
        addresses2.setGlobalId(1000002L);
        addresses2.setAoLevel("3");
        addressesList.add(addresses2);

        Addresses addresses3 = new Addresses();
        addresses3.setGlobalId(1000003L);
        addresses3.setAoLevel("7");
        addresses3.setStreetCode("STREET_CODE_1");
        addresses3.setPlanCode("PLAN_CODE_1");
        addresses3.setPlaceCode("PLACECODE_CODE_1");
        addresses3.setAreaCode("AREA_CODE_1");
        addresses3.setAreaCodeOmkTe("AREA_OMKTE_CODE_1");
        addressesList.add(addresses3);

        List<AddressRegistryBaseType> addressRegistryBaseTypes = new ArrayList<>();

        AddressRegistryBaseType addressRegistryBaseType1 = new AddressRegistryBaseType();
        addressRegistryBaseType1.setAoLevel("8");
        Street street = new Street();
        street.setCode("STREET_CODE_1");
        addressRegistryBaseType1.setStreet(street);
        Plan plan = new Plan();
        plan.setCode("PLAN_CODE_1");
        addressRegistryBaseType1.setPlan(plan);
        Place place = new Place();
        place.setCode("PLACECODE_CODE_1");
        addressRegistryBaseType1.setPlace(place);
        Area area = new Area();
        area.setCode("AREA_CODE_1");
        addressRegistryBaseType1.setArea(area);
        AreaOMKTE areaOMKTE = new AreaOMKTE();
        areaOMKTE.setCode("AREA_OMKTE_CODE_1;AREA_OMKTE_CODE_2");
        addressRegistryBaseType1.setAreaOMKTE(areaOMKTE);
        RegionOMKTE regionOMKTE = new RegionOMKTE();
        regionOMKTE.setCode("REGION_OMKTE_CODE_1;REGION_OMKTE_CODE_2;REGION_OMKTE_CODE_3");
        addressRegistryBaseType1.setRegionOMKTE(regionOMKTE);
        addressRegistryBaseTypes.add(addressRegistryBaseType1);

        List<Addresses> crossAddresses =
                algorithms.findIntersectingAddressesAdd(addressRegistryBaseTypes, addressesList, validation);

        assertEquals(crossAddresses.size(), 1);
    }

    // 2 -> b.1 -> 2.1 - возврат 2 адресов
    // Поиск по переданным REGION_TE_CODE(3 кода), AREACODE_OMK_TE(2 кода переданы), AREACODE, PLACECODE, PLANCODE, STREETCODE и AOLEVEL=7
    @Test
    public void input_address_level_8_13() {
        Validation validation = new Validation();

        List<Addresses> addressesList = new ArrayList<>();

        Addresses addresses = new Addresses();
        addresses.setGlobalId(1000000L);
        addresses.setAoLevel("7");
        addresses.setStreetCode("STREET_CODE_1");
        addressesList.add(addresses);

        Addresses addresses1 = new Addresses();
        addresses1.setGlobalId(1000001L);
        addresses1.setAoLevel("7");
        addresses1.setStreetCode("STREET_CODE_1");
        addresses1.setPlanCode("PLAN_CODE_1");
        addresses1.setPlaceCode("PLACECODE_CODE_1");
        addresses1.setAreaCode("AREA_CODE_1");
        addresses1.setAreaCodeOmkTe("AREA_OMKTE_CODE_2");
        addresses1.setRegionTeCode("REGION_OMKTE_CODE_1");
        addressesList.add(addresses1);

        Addresses addresses2 = new Addresses();
        addresses2.setGlobalId(1000002L);
        addresses2.setAoLevel("3");
        addressesList.add(addresses2);

        Addresses addresses3 = new Addresses();
        addresses3.setGlobalId(1000003L);
        addresses3.setAoLevel("7");
        addresses3.setStreetCode("STREET_CODE_1");
        addresses3.setPlanCode("PLAN_CODE_1");
        addresses3.setPlaceCode("PLACECODE_CODE_1");
        addresses3.setAreaCode("AREA_CODE_1");
        addresses3.setAreaCodeOmkTe("AREA_OMKTE_CODE_1");
        addresses3.setRegionTeCode("REGION_OMKTE_CODE_3");
        addressesList.add(addresses3);

        List<AddressRegistryBaseType> addressRegistryBaseTypes = new ArrayList<>();

        AddressRegistryBaseType addressRegistryBaseType1 = new AddressRegistryBaseType();
        addressRegistryBaseType1.setAoLevel("8");
        Street street = new Street();
        street.setCode("STREET_CODE_1");
        addressRegistryBaseType1.setStreet(street);
        Plan plan = new Plan();
        plan.setCode("PLAN_CODE_1");
        addressRegistryBaseType1.setPlan(plan);
        Place place = new Place();
        place.setCode("PLACECODE_CODE_1");
        addressRegistryBaseType1.setPlace(place);
        Area area = new Area();
        area.setCode("AREA_CODE_1");
        addressRegistryBaseType1.setArea(area);
        AreaOMKTE areaOMKTE = new AreaOMKTE();
        areaOMKTE.setCode("AREA_OMKTE_CODE_1;AREA_OMKTE_CODE_2");
        addressRegistryBaseType1.setAreaOMKTE(areaOMKTE);
        RegionOMKTE regionOMKTE = new RegionOMKTE();
        regionOMKTE.setCode("REGION_OMKTE_CODE_1;REGION_OMKTE_CODE_2;REGION_OMKTE_CODE_3");
        addressRegistryBaseType1.setRegionOMKTE(regionOMKTE);
        addressRegistryBaseTypes.add(addressRegistryBaseType1);

        List<Addresses> crossAddresses =
                algorithms.findIntersectingAddressesAdd(addressRegistryBaseTypes, addressesList, validation);

        assertEquals(crossAddresses.size(), 2);
    }

    // TODO Test -> 2-> b.2. .....

}
