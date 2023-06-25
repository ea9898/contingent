package area.service.methods;

import area.service.MockConfiguration;
import area.service.PersistenceConfiguration;
import liquibase.Liquibase;
import liquibase.database.Database;
import liquibase.database.DatabaseFactory;
import liquibase.database.jvm.JdbcConnection;
import liquibase.exception.LiquibaseException;
import liquibase.resource.FileSystemResourceAccessor;
import moscow.ptnl.contingent.domain.area.Algorithms;
import moscow.ptnl.contingent.domain.area.AreaService;
import moscow.ptnl.contingent.domain.area.entity.Addresses;
import moscow.ptnl.contingent.domain.area.entity.MoAddress;
import moscow.ptnl.contingent.nsi.domain.area.AreaType;
import moscow.ptnl.contingent.nsi.domain.area.MedicalOrganisationsOnko;
import moscow.ptnl.contingent.nsi.domain.repository.MedicalOrganisationsOnkoRepository;
import org.junit.jupiter.api.Assertions;
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
import ru.mos.emias.contingent2.area.v4.Fault;
import ru.mos.emias.contingent2.area.v4.types.GetAreaByIdRequest;
import ru.mos.emias.contingent2.area.v4.types.GetAreaByIdResponse;
import service.BaseTest;

import javax.sql.DataSource;
import java.sql.SQLException;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.doReturn;

@ExtendWith(SpringExtension.class)
@ExtendWith(MockitoExtension.class)
@ContextConfiguration(classes = {PersistenceConfiguration.class, MockConfiguration.class})
@Transactional
public class GetAreaByIdTest {

    @Autowired
    private ru.mos.emias.contingent2.area.v4.AreaPT areaServiceV4;

    @Autowired
    public MedicalOrganisationsOnkoRepository medicalOrganisationsOnkoRepository;

    @BeforeAll
    public static void init(@Qualifier("contingentDataSource") DataSource dataSource) throws LiquibaseException, SQLException {
        Database database = DatabaseFactory.getInstance().findCorrectDatabaseImplementation(new JdbcConnection(dataSource.getConnection()));
        Liquibase liquibase = new Liquibase("changelog/area/versions/master.xml", new FileSystemResourceAccessor("../database"), database);
        liquibase.update("");
    }

    @Test
    @Sql(scripts = {"/sql/getAreaById.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void test1() {
        MedicalOrganisationsOnko medicalOrganisationsOnko = new MedicalOrganisationsOnko();
        medicalOrganisationsOnko.setMoId(1L);
        medicalOrganisationsOnko.setCodeOncoArea("123");
        doReturn(Optional.of(medicalOrganisationsOnko)).when(medicalOrganisationsOnkoRepository).findByMoId(medicalOrganisationsOnko.getMoId());

        GetAreaByIdRequest request = new GetAreaByIdRequest();
        request.setAreaId(1);
        GetAreaByIdResponse response = null;
        try {
            response = areaServiceV4.getAreaById(request);
        } catch (Fault e) {
            throw new RuntimeException(e);
        }
        Assertions.assertNotNull(response);
        Assertions.assertEquals("111", response.getResult().getSpecialNumber());
        Assertions.assertEquals(333, response.getResult().getAttFinalLimit());
        Assertions.assertEquals(444, response.getResult().getAttInfoLimit());
        Assertions.assertEquals(0, response.getResult().getMedicalEmployees().getMedicalEmployees().get(0).getEmployeeCategory());
//        Assertions.assertEquals(1 response.getResult().getMedicalEmployees().getReplacementCount());
//        Assertions.assertEquals(222, response.getResult().getAreaTypeCategory());
//        Assertions.assertEquals(555, response.getResult().getResidentsBindRate());
    }
}
