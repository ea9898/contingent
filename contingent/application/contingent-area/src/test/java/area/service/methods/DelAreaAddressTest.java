package area.service.methods;

import area.service.MockConfiguration;
import area.service.PersistenceConfiguration;
import liquibase.Liquibase;
import liquibase.database.Database;
import liquibase.database.DatabaseFactory;
import liquibase.database.jvm.JdbcConnection;
import liquibase.exception.LiquibaseException;
import liquibase.resource.FileSystemResourceAccessor;
import moscow.ptnl.contingent.area.ws.v3.AreaServiceImpl;
import moscow.ptnl.contingent.domain.area.Algorithms;
import moscow.ptnl.contingent.domain.area.AreaService;
import moscow.ptnl.contingent.domain.area.entity.AreaAddress;
import moscow.ptnl.contingent.domain.area.repository.AreaAddressRepository;
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
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

@ExtendWith(SpringExtension.class)
@ExtendWith(MockitoExtension.class)
@ContextConfiguration(classes = {PersistenceConfiguration.class, MockConfiguration.class})
@Transactional
public class DelAreaAddressTest extends service.BaseTest {

    @Autowired
    private AreaAddressRepository areaAddressRepository;

    @Autowired
    private AreaService areaService;

    @BeforeAll
    public static void init(@Qualifier("contingentDataSource") DataSource dataSource) throws LiquibaseException, SQLException {
        Database database = DatabaseFactory.getInstance().findCorrectDatabaseImplementation(new JdbcConnection(dataSource.getConnection()));
        Liquibase liquibase = new Liquibase("changelog/area/versions/master.xml", new FileSystemResourceAccessor("../database"), database);
        liquibase.update("");
    }

    @Test
    @Sql(scripts = {"/sql/delAreaAddressFindActualServiceAreas.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void deleteAreaAddresses() throws ContingentException {
        List<Long> areaAddressIds = new ArrayList<>();
        areaAddressIds.add(1L);
        areaAddressIds.add(2L);

        areaService.delAreaAddress(1, areaAddressIds);
    }

    @Test
    @Sql(scripts = {"/sql/delAreaAddressFindActualServiceAreas.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void deleteAreaAddressesEmptyList() throws ContingentException {
        List<Long> areaAddressIds = new ArrayList<>();

        areaService.delAreaAddress(1, areaAddressIds);
    }

    @Test
    @Sql(scripts = {"/sql/delAreaAddressFindActualServiceAreas.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void deleteAreaAddressesOneNotExist() {
        List<Long> areaAddressIds = new ArrayList<>();
        areaAddressIds.add(1L);
        areaAddressIds.add(2L);
        areaAddressIds.add(3L);
        Throwable exception = assertThrows(ContingentException.class, () ->  areaService.delAreaAddress(1, areaAddressIds));
        assertEquals("Территория обслуживания с ИД 3 не существует или уже перенесена в архив или не соответствует ИД участка", exception.getMessage());
    }

    @Test
    @Sql(scripts = {"/sql/delAreaAddressFindActualServiceAreas.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void deleteAreaAddresseAreaOther() {
        List<Long> areaAddressIds = new ArrayList<>();
        areaAddressIds.add(1L);
        areaAddressIds.add(2L);

        Throwable exception = assertThrows(ContingentException.class, () ->  areaService.delAreaAddress(2, areaAddressIds));
        assertEquals("Территория обслуживания с ИД 1 не существует или уже перенесена в архив или не соответствует ИД участка", exception.getMessage());
    }

    @Test
    @Sql(scripts = {"/sql/delAreaAddressFindActualServiceAreas.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void deleteAreaAddresseAreaNotExist() {
        List<Long> areaAddressIds = new ArrayList<>();
        areaAddressIds.add(1L);
        areaAddressIds.add(2L);

        Throwable exception = assertThrows(ContingentException.class, () ->  areaService.delAreaAddress(3, areaAddressIds));
        assertEquals("Участок обслуживания МО с ИД 3 не найден в системе", exception.getMessage());
    }
}
