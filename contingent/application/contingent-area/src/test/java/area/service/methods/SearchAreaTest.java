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
import moscow.ptnl.contingent.domain.area.model.area.AreaInfo;
import moscow.ptnl.contingent.area.service.AreaServiceInternal;
import moscow.ptnl.contingent.domain.area.model.area.MedicalEmployee;
import moscow.ptnl.contingent.domain.area.model.area.SearchAreaAddress;
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
import ru.mos.emias.contingent2.area.types.SearchAreaRequest;

import javax.sql.DataSource;
import java.sql.SQLException;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

@ExtendWith(SpringExtension.class)
@ExtendWith(MockitoExtension.class)
@ContextConfiguration(classes= {PersistenceConfiguration.class, MockConfiguration.class})
@Transactional
public class SearchAreaTest {

    @Autowired
    private AreaService areaServiceDomain;

    private static final PageRequest PR = PageRequest.of(0, 10);
    private static final List EL = Collections.emptyList();

    @BeforeAll
    public static void init(@Qualifier("contingentDataSource") DataSource dataSource) throws LiquibaseException, SQLException {
        Database database = DatabaseFactory.getInstance().findCorrectDatabaseImplementation(new JdbcConnection(dataSource.getConnection()));
        Liquibase liquibase = new Liquibase("changelog/area/versions/master.xml", new FileSystemResourceAccessor("../database"), database);
        liquibase.update("");
    }

    @Test
    public void searchAreaExceptionTest() {
        Throwable exception = assertThrows(ContingentException.class, () -> areaServiceDomain.searchArea(null, null, EL,
                EL, null, null, null, EL, EL, null, null));
        assertEquals(exception.getMessage(), "Не заданы критерии поиска");
    }

    @Test
    @Sql(scripts = {"/sql/areaTypeClass.sql", "/sql/searchAreaTest.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void searchAreaAllParametersTest() {
        Page<AreaInfo> areas = assertDoesNotThrow(() -> areaServiceDomain.searchArea(
                1L, 204L, EL, Collections.singletonList(10L),
                123, null, false, EL, EL, null, PR));
        assertNotNull(areas);
        assertEquals(areas.getNumberOfElements(), 1);
        assertEquals(areas.getNumberOfElements(), areas.getContent().size());
        assertEquals(areas.getContent().get(0).getArea().getId(), (Long) 2L);
    }

    @Test
    @Sql(scripts = {"/sql/areaTypeClass.sql", "/sql/searchAreaTest.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void searchAreaByMedicalEmployeesTest() {
        Page<AreaInfo> areas = assertDoesNotThrow(() -> areaServiceDomain.searchArea(
                null, null, EL, Collections.singletonList(10L),
                null, null, false, Arrays.asList(new MedicalEmployee() {{setMedicalEmployeeJobId(123L);}},
                        new MedicalEmployee() {{setSnils("snilscode1");}}), EL, null, PR));
        assertNotNull(areas);
        assertEquals(areas.getNumberOfElements(), 1);
        assertEquals(areas.getNumberOfElements(), areas.getContent().size());
        assertEquals(areas.getContent().get(0).getArea().getId(), (Long) 2L);
    }

    @Test
    @Sql(scripts = {"/sql/areaTypeClass.sql", "/sql/searchAreaTest.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void searchAreaByAddressesExactMatchTest() {
        Page<AreaInfo> areas = assertDoesNotThrow(() -> areaServiceDomain.searchArea(
                null, null, EL, Collections.singletonList(10L),
                null, null, false, EL,
                Arrays.asList(new SearchAreaAddress() {{setGlobalIdNsi(111L); setAreaOMKTEcode(""); setRegionOMKTEcode("");}}),
                true, PR));
        assertNotNull(areas);
        assertEquals(areas.getNumberOfElements(), 1);
        assertEquals(areas.getNumberOfElements(), areas.getContent().size());
        assertEquals(areas.getContent().get(0).getArea().getId(), (Long) 2L);
    }
}
