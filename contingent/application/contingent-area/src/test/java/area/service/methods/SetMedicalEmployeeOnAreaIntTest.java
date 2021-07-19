package area.service.methods;

import area.service.MockConfiguration;
import area.service.PersistenceConfiguration;
import liquibase.Liquibase;
import liquibase.database.Database;
import liquibase.database.DatabaseFactory;
import liquibase.database.jvm.JdbcConnection;
import liquibase.exception.LiquibaseException;
import liquibase.resource.FileSystemResourceAccessor;
import moscow.ptnl.contingent.PersistenceConstraint;
import moscow.ptnl.contingent.domain.area.AreaService;
import moscow.ptnl.contingent.domain.area.model.area.AddMedicalEmployee;
import moscow.ptnl.contingent.error.ContingentException;
import moscow.ptnl.contingent.repository.area.AreaMedicalEmployeeCRUDRepository;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.test.annotation.DirtiesContext;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.jdbc.Sql;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.transaction.annotation.Transactional;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.sql.DataSource;
import java.sql.SQLException;
import java.time.LocalDate;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * Проверка работы с СУПП кодами п. 6.3.1. и 6.6
 */
@ExtendWith(SpringExtension.class)
@ExtendWith(MockitoExtension.class)
@ContextConfiguration(classes= {PersistenceConfiguration.class, MockConfiguration.class})
@DirtiesContext(classMode = DirtiesContext.ClassMode.AFTER_EACH_TEST_METHOD)
@Transactional
public class SetMedicalEmployeeOnAreaIntTest {

    @Autowired
    private AreaService areaService;

    @Autowired
    private AreaMedicalEmployeeCRUDRepository areaMedicalEmployeeCRUDRepository;

    @PersistenceContext(unitName = PersistenceConstraint.PU_CONTINGENT_NAME)
    protected EntityManager entityManager;

    @BeforeAll
    public static void init(@Qualifier("contingentDataSource") DataSource dataSource) throws LiquibaseException, SQLException {
        Database database = DatabaseFactory.getInstance().findCorrectDatabaseImplementation(new JdbcConnection(dataSource.getConnection()));
        Liquibase liquibase = new Liquibase("changelog/area/versions/master.xml", new FileSystemResourceAccessor("../database"), database);
        liquibase.update("");
    }

    @Test
    @Sql(scripts = {"/sql/areaTypeClass.sql", "/sql/searchAreaTest.sql", "/sql/positionCodeAndCoTest.sql"},
            executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void getPositionCodeFromInputTest() {
        List<Long> results = Assertions.assertDoesNotThrow(() -> areaService.setMedicalEmployeeOnAreaInternal(4, Arrays.asList(
                new AddMedicalEmployee() {{ setMedicalEmployeeJobInfoId(12L); setPositionCode("test1"); setStartDate(LocalDate.now()); }}
        ), Collections.emptyList()));
    }

    @Test
    @Sql(scripts = {"/sql/areaTypeClass.sql", "/sql/searchAreaTest.sql", "/sql/positionCodeAndCoTest.sql"},
            executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void getPositionCodeFromMapTest() {
        List<Long> results = Assertions.assertDoesNotThrow(() -> areaService.setMedicalEmployeeOnAreaInternal(4, Arrays.asList(
                new AddMedicalEmployee() {{ setMedicalEmployeeJobInfoId(12L); setPositionCode("135"); setStartDate(LocalDate.now()); }}
        ), Collections.emptyList()));
    }

    @Test
    @Sql(scripts = {"/sql/areaTypeClass.sql", "/sql/searchAreaTest.sql", "/sql/positionCodeAndCoTest.sql"},
            executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void getPositionCodeFromInputErrorTest() {
        ContingentException result = Assertions.assertThrows(ContingentException.class, () -> areaService.setMedicalEmployeeOnAreaInternal(4, Arrays.asList(
                new AddMedicalEmployee() {{ setMedicalEmployeeJobInfoId(12L); setPositionCode("test5"); setStartDate(LocalDate.now()); }}
        ), Collections.emptyList()));
        Assertions.assertEquals("Код должности медработника test5 не найден в системе", result.getMessage());
    }

    @Test
    @Sql(scripts = {"/sql/areaTypeClass.sql", "/sql/searchAreaTest.sql", "/sql/positionCodeAndCoTest.sql"},
            executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void getPositionCodeFromMapErrorTest() {
        ContingentException result = Assertions.assertThrows(ContingentException.class, () -> areaService.setMedicalEmployeeOnAreaInternal(4, Arrays.asList(
                new AddMedicalEmployee() {{ setMedicalEmployeeJobInfoId(12L); setPositionCode("1357"); setStartDate(LocalDate.now()); }}
        ), Collections.emptyList()));
        Assertions.assertEquals("Код должности медработника 1357 не найден в системе", result.getMessage());
    }
}
