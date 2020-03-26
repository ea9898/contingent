package area.service.methods;

import area.service.MockConfiguration;
import area.service.PersistenceConfiguration;
import liquibase.Liquibase;
import liquibase.database.Database;
import liquibase.database.DatabaseFactory;
import liquibase.database.jvm.JdbcConnection;
import liquibase.exception.LiquibaseException;
import liquibase.resource.FileSystemResourceAccessor;
import moscow.ptnl.contingent.domain.area.OrderService;
import moscow.ptnl.contingent.domain.area.entity.AddressAllocationOrders;
import moscow.ptnl.contingent.error.ContingentException;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.jdbc.Sql;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.transaction.annotation.Transactional;

import javax.sql.DataSource;
import java.sql.SQLException;
import java.time.LocalDate;

import static org.junit.jupiter.api.Assertions.*;

@ExtendWith(SpringExtension.class)
@ExtendWith(MockitoExtension.class)
@ContextConfiguration(classes= {PersistenceConfiguration.class, MockConfiguration.class})
@Transactional
public class SearchOrderTest {

    @Autowired
    private OrderService orderService;

    private static final PageRequest PR = PageRequest.of(0, 10);

    @BeforeAll
    public static void init(@Qualifier("contingentDataSource") DataSource dataSource) throws LiquibaseException, SQLException {
        Database database = DatabaseFactory.getInstance().findCorrectDatabaseImplementation(new JdbcConnection(dataSource.getConnection()));
        Liquibase liquibase = new Liquibase("changelog/area/versions/master.xml", new FileSystemResourceAccessor("../database"), database);
        liquibase.update("");
    }

    @Test
    public void searchOrderExceptionTest() {
        Throwable exception = assertThrows(ContingentException.class, () -> orderService.searchOrder(null, null, null, null, null));
        assertEquals(exception.getMessage(), "Не заданы критерии поиска");
    }

    @Test
    @Sql(scripts = {"/sql/searchOrderTest.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void searchOrderAllParametersTest() {
        Page<AddressAllocationOrders> orders = assertDoesNotThrow(() -> orderService.searchOrder(2L, "2", LocalDate.now(), "name", PR));
        assertNotNull(orders);
        assertEquals(orders.getNumberOfElements(), 1);
        assertEquals(orders.getNumberOfElements(), orders.getContent().size());
        assertEquals(orders.getContent().get(0).getId(), (Long) 2L);
    }

    @Test
    @Sql("/sql/searchOrderTest.sql")
    public void searchOrderMultipleTest() {
        Page<AddressAllocationOrders> orders = assertDoesNotThrow(() -> orderService.searchOrder(null, null, LocalDate.now(), "name", PR));
        assertNotNull(orders);
        assertEquals(orders.getNumberOfElements(), 2);
        assertEquals(orders.getNumberOfElements(), orders.getContent().size());
        assertTrue(orders.get().map(AddressAllocationOrders::getName).anyMatch("name"::equals));
        assertTrue(orders.get().map(AddressAllocationOrders::getName).anyMatch("name3"::equals));
    }

    @Test
    @Sql("/sql/searchOrderTest.sql")
    public void searchOrderByIdTest() {
        Page<AddressAllocationOrders> orders = assertDoesNotThrow(() -> orderService.searchOrder(3L, null, null, null, PR));
        assertNotNull(orders);
        assertEquals(orders.getNumberOfElements(), 1);
        assertEquals(orders.getNumberOfElements(), orders.getContent().size());
        assertEquals(orders.getContent().get(0).getId(), (Long) 3L);
    }

    @Test
    @Sql("/sql/searchOrderTest.sql")
    public void searchOrderByNumberTest() {
        Page<AddressAllocationOrders> orders = assertDoesNotThrow(() -> orderService.searchOrder(null, "3", null, null, PR));
        assertNotNull(orders);
        assertEquals(orders.getNumberOfElements(), 1);
        assertEquals(orders.getNumberOfElements(), orders.getContent().size());
        assertEquals(orders.getContent().get(0).getId(), (Long) 3L);
    }

    @Test
    @Sql("/sql/searchOrderTest.sql")
    public void searchOrderNotFoundArchivedTest() {
        Page<AddressAllocationOrders> orders = assertDoesNotThrow(() -> orderService.searchOrder(null, "4", null, null, PR));
        assertNotNull(orders);
        assertEquals(orders.getNumberOfElements(), 0);
        assertEquals(orders.getNumberOfElements(), orders.getContent().size());
    }
}
