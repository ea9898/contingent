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
import moscow.ptnl.contingent.domain.area.model.area.AreaHistory;
import moscow.ptnl.contingent.error.ContingentException;
import org.junit.jupiter.api.Assertions;
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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.fail;

@ExtendWith(SpringExtension.class)
@ExtendWith(MockitoExtension.class)
@ContextConfiguration(classes= {PersistenceConfiguration.class, MockConfiguration.class})
@Transactional
public class GetAreaHistoryTest {

    private static final PageRequest PR = PageRequest.of(0, 10);

    @Autowired
    private AreaService areaServiceDomain;

    @BeforeAll
    public static void init(@Qualifier("contingentDataSource") DataSource dataSource) throws LiquibaseException, SQLException {
        Database database = DatabaseFactory.getInstance().findCorrectDatabaseImplementation(new JdbcConnection(dataSource.getConnection()));
        Liquibase liquibase = new Liquibase("changelog/area/versions/master.xml", new FileSystemResourceAccessor("../database"), database);
        liquibase.update("");
    }

    @Test
    public void getAreaHistoryAreaNotFound() {
        Throwable exception = assertThrows(ContingentException.class, () -> areaServiceDomain.getAreaHistory(1L, null));
        assertEquals(exception.getMessage(), "Участок обслуживания МО с ИД 1 не найден в системе");
    }

    @Test
    @Sql(scripts = {"/sql/areaHistory.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void getAreaHistoryCorrect() {
        AreaHistory areaHistoryPage = new AreaHistory();
        try {
            areaHistoryPage = areaServiceDomain.getAreaHistory(100002165L, null);
        } catch (ContingentException e) {
            fail("Ошибка " + e.getMessage());
        };
        Assertions.assertEquals(areaHistoryPage.getEvents().getTotalElements(), 5L);
    }
}
