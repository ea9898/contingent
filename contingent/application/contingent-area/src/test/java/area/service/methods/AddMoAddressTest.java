package area.service.methods;

import area.service.MockConfiguration;
import area.service.PersistenceConfiguration;
import liquibase.Liquibase;
import liquibase.database.Database;
import liquibase.database.DatabaseFactory;
import liquibase.database.jvm.JdbcConnection;
import liquibase.exception.LiquibaseException;
import liquibase.resource.FileSystemResourceAccessor;
import moscow.ptnl.contingent.domain.area.AreaService;
import moscow.ptnl.contingent.domain.area.entity.AddressAllocationOrders;
import moscow.ptnl.contingent.domain.area.model.area.AddressRegistry;
import moscow.ptnl.contingent.error.ContingentException;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.jdbc.Sql;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.transaction.annotation.Transactional;

import javax.sql.DataSource;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.*;

@ExtendWith(SpringExtension.class)
@ExtendWith(MockitoExtension.class)
@ContextConfiguration(classes = {PersistenceConfiguration.class, MockConfiguration.class})
@Transactional
public class AddMoAddressTest {

    @Autowired
    private AreaService areaService;

    @BeforeAll
    public static void init(@Qualifier("contingentDataSource") DataSource dataSource) throws LiquibaseException, SQLException {
        Database database = DatabaseFactory.getInstance().findCorrectDatabaseImplementation(new JdbcConnection(dataSource.getConnection()));
        Liquibase liquibase = new Liquibase("changelog/area/versions/master.xml", new FileSystemResourceAccessor("../database"), database);
        liquibase.update("");
    }
    // --------------- 2 шаг ---------------- //
    @Test
    public void addMoAddressesTestAmountAddressesMoreThenPar1() {
        List<AddressRegistry> addressRegistryList = new ArrayList<>();
        for (int i = 0; i < 100; i++) {
            addressRegistryList.add(new AddressRegistry());
        }
        Throwable exception = assertThrows(ContingentException.class, () -> areaService.addMoAddress(0, Collections.emptyList(), 0, addressRegistryList, true));
        assertEquals("Превышено максимально допустимое количество адресов для распределения (не более 0)", exception.getMessage());
    }

    // --------------- 3 шаг ---------------- //
    @Test
    public void addMoAddressesTestAreaTypeEmptyDB() {
        List<AddressRegistry> addressRegistryList = new ArrayList<>();
        for (int i = 0; i < 100; i++) {
            addressRegistryList.add(new AddressRegistry());
        }
        List<Long> areaTypeCode = new ArrayList<>();
        areaTypeCode.add(1L);

        Throwable exception = assertThrows(ContingentException.class, () -> areaService.addMoAddress(0, areaTypeCode, 0, addressRegistryList, false));
        assertEquals("Тип участка с ИД: 1 не найден в системе", exception.getMessage());
    }

    @Test
    @Sql(scripts = {"/sql/areaTypeCode2.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addMoAddressesTestAreaTypeOneExistOneNotExist() {
        List<AddressRegistry> addressRegistryList = new ArrayList<>();
        for (int i = 0; i < 100; i++) {
            addressRegistryList.add(new AddressRegistry());
        }
        List<Long> areaTypeCode = new ArrayList<>();
        areaTypeCode.add(1L);
        areaTypeCode.add(2L);

        Throwable exception = assertThrows(ContingentException.class, () -> areaService.addMoAddress(0, areaTypeCode, 0, addressRegistryList, false));
        assertEquals("Тип участка с ИД: 1 не найден в системе", exception.getMessage());
    }

    @Test
    @Sql(scripts = {"/sql/areaTypeCodeArchived.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addMoAddressesTestAreaTypeOneArchivedOneNotArchived() {
        List<AddressRegistry> addressRegistryList = new ArrayList<>();
        for (int i = 0; i < 100; i++) {
            addressRegistryList.add(new AddressRegistry());
        }
        List<Long> areaTypeCode = new ArrayList<>();
        areaTypeCode.add(1L);
        areaTypeCode.add(2L);

        Throwable exception = assertThrows(ContingentException.class, () -> areaService.addMoAddress(0, areaTypeCode, 0, addressRegistryList, false));
        assertEquals("Тип участка с ИД: 1 не найден в системе", exception.getMessage());
    }

    // --------------- 4 шаг ---------------- //
    @Test
    @Sql(scripts = {"/sql/addressAllocationOrders.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addMoAddressesTestOrderIdExist() {
        List<AddressRegistry> addressRegistryList = new ArrayList<>();
        for (int i = 0; i < 100; i++) {
            addressRegistryList.add(new AddressRegistry());
        }

        AddressAllocationOrders orders = new AddressAllocationOrders();
        orders.setId(1L);
        orders.setNumber("93691");
        orders.setName("Name.Order.Update");

        Throwable exception = assertThrows(ContingentException.class, () -> areaService.addMoAddress(0, Collections.emptyList(), 3, addressRegistryList, false));
        assertEquals("Распоряжение с ИД 3 не найдено в системе", exception.getMessage());
    }

    @Test
    @Sql(scripts = {"/sql/addressAllocationOrders.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addMoAddressesTestOrderIdArchived() {
        List<AddressRegistry> addressRegistryList = new ArrayList<>();
        for (int i = 0; i < 100; i++) {
            addressRegistryList.add(new AddressRegistry());
        }

        AddressAllocationOrders orders = new AddressAllocationOrders();
        orders.setId(2L);
        orders.setNumber("93691");
        orders.setName("Name.Order.Update");

        Throwable exception = assertThrows(ContingentException.class, () -> areaService.addMoAddress(0, Collections.emptyList(), 2, addressRegistryList, false));
        assertEquals("Невозможно добавить адрес, т.к. не указан его уровень", exception.getMessage());
    }

    // --------------- 5 шаг ---------------- //
}
