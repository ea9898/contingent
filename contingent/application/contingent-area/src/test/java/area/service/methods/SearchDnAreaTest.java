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
import moscow.ptnl.contingent.domain.area.entity.Area;
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
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

@ExtendWith(SpringExtension.class)
@ExtendWith(MockitoExtension.class)
@ContextConfiguration(classes= {PersistenceConfiguration.class, MockConfiguration.class})
@Transactional
public class SearchDnAreaTest {

    @Autowired
    private AreaService areaService;

    private static final PageRequest PR = PageRequest.of(0, 10);
    private static final List EL = Collections.emptyList();

    @BeforeAll
    public static void init(@Qualifier("contingentDataSource") DataSource dataSource) throws LiquibaseException, SQLException {
        Database database = DatabaseFactory.getInstance().findCorrectDatabaseImplementation(new JdbcConnection(dataSource.getConnection()));
        Liquibase liquibase = new Liquibase("changelog/area/versions/master.xml", new FileSystemResourceAccessor("../database"), database);
        liquibase.update("");
    }

    @Test
    public void searchDnAreaExceptionTest() {
        Throwable exception = assertThrows(ContingentException.class, () -> areaService.searchDnArea(null, EL, EL, EL, EL, PR));
        assertEquals(exception.getMessage(), "Не заданы критерии поиска");
    }

    @Test
    @Sql(scripts = {"/sql/areaTypeClass.sql", "/sql/searchAreaTest.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void searchDnAreaAllParametersTest() {
        Page<Area> areas = assertDoesNotThrow(() -> areaService.searchDnArea(
                204L, Collections.singletonList(100L), Collections.singletonList(10L), Collections.singletonList(49L), EL, PR));
        assertNotNull(areas);
        assertEquals(1, areas.getNumberOfElements());
        assertEquals(areas.getNumberOfElements(), areas.getContent().size());
        assertEquals((Long) 2L, areas.getContent().get(0).getId());
    }

    @Test
    @Sql(scripts = {"/sql/areaTypeClass.sql", "/sql/searchAreaTest.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void searchDnAreaByAreaIdsTest() {
        Page<Area> areas = assertDoesNotThrow(() -> areaService.searchDnArea(null, EL, EL, EL, Collections.singletonList(8L), PR));
        assertNotNull(areas);
        assertEquals(1, areas.getNumberOfElements());
        assertEquals(areas.getNumberOfElements(), areas.getContent().size());
        assertEquals((Long) 8L, areas.getContent().get(0).getId());
    }

    @Test
    @Sql(scripts = {"/sql/areaTypeClass.sql", "/sql/searchAreaTest.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void searchDnArea_CONTINGENT2_638_Test() {
        Page<Area> areas = assertDoesNotThrow(() -> areaService.searchDnArea(136L, EL, EL, EL, EL, PR));
        assertEquals(1, areas.getNumberOfElements());
    }
}
