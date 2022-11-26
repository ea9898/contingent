package area.service.algorithms;

import area.service.MockConfiguration;
import area.service.PersistenceConfiguration;
import liquibase.Liquibase;
import liquibase.database.Database;
import liquibase.database.DatabaseFactory;
import liquibase.database.jvm.JdbcConnection;
import liquibase.exception.LiquibaseException;
import liquibase.resource.FileSystemResourceAccessor;
import moscow.ptnl.contingent.domain.area.Algorithms;
import moscow.ptnl.contingent.domain.area.entity.Addresses;
import moscow.ptnl.contingent.domain.area.entity.MoAddress;
import moscow.ptnl.contingent.nsi.domain.area.AreaType;
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
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.Assertions.assertTrue;

@ExtendWith(SpringExtension.class)
@ExtendWith(MockitoExtension.class)
@ContextConfiguration(classes = {PersistenceConfiguration.class, MockConfiguration.class})
@Transactional
public class A_YY_15_Test {

    @Autowired
    private Algorithms algorithms;

    @BeforeAll
    public static void init(@Qualifier("contingentDataSource") DataSource dataSource) throws LiquibaseException, SQLException {
        Database database = DatabaseFactory.getInstance().findCorrectDatabaseImplementation(new JdbcConnection(dataSource.getConnection()));
        Liquibase liquibase = new Liquibase("changelog/area/versions/master.xml", new FileSystemResourceAccessor("../database"), database);
        liquibase.update("");
    }

    @Test
    @Sql(scripts = {"/sql/moAddresses.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addMoAddressesTest() {
        Addresses addresses = new Addresses();
        addresses.setId(1L);
        addresses.setGlobalId(111L);
        addresses.setAoLevel("8");

        AreaType areaType = new AreaType();
        areaType.setCode(199L);

        MoAddress moAddress = new MoAddress();
        moAddress.setId(1L);
        moAddress.setMoId(20L);
        moAddress.setAreaType(areaType);
        moAddress.setAddress(addresses);

        List<Long> moAddresses = algorithms.searchServiceDistrictMOByAddressV33(areaType, addresses.getGlobalId());

        assertEquals(1, moAddresses.size());
        assertEquals(20L, moAddresses.get(0).longValue());

    }

    @Test
    @Sql(scripts = {"/sql/addMoAddressesAoLevel8-7.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addMoAddressesAoLevel8AoLevel7() {
        AreaType areaType = new AreaType();
        areaType.setCode(199L);

        List<Long> moAddresses = algorithms.searchServiceDistrictMOByAddressV33(areaType, 10L);

        assertEquals(1, moAddresses.size());
        assertEquals(20L, moAddresses.get(0).longValue());
    }

    @Test
    @Sql(scripts = {"/sql/addMoAddressesAoLevel8-65.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addMoAddressesAoLevel8AoLevel65() {
        AreaType areaType = new AreaType();
        areaType.setCode(199L);

        List<Long> moAddresses = algorithms.searchServiceDistrictMOByAddressV33(areaType, 10L);

        assertEquals(1, moAddresses.size());
        assertEquals(20L, moAddresses.get(0).longValue());
    }

    @Test
    @Sql(scripts = {"/sql/addMoAddressesAoLevel8-6.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addMoAddressesAoLevel8AoLevel6() {
        AreaType areaType = new AreaType();
        areaType.setCode(199L);

        List<Long> moAddresses = algorithms.searchServiceDistrictMOByAddressV33(areaType, 10L);

        assertEquals(1, moAddresses.size());
        assertEquals(20L, moAddresses.get(0).longValue());
    }

    @Test
    @Sql(scripts = {"/sql/addMoAddressesAoLevel8-4.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addMoAddressesAoLevel8AoLevel4() {
        AreaType areaType = new AreaType();
        areaType.setCode(199L);

        List<Long> moAddresses = algorithms.searchServiceDistrictMOByAddressV33(areaType, 10L);

        assertEquals(1, moAddresses.size());
        assertEquals(20L, moAddresses.get(0).longValue());
    }

    @Test
    @Sql(scripts = {"/sql/addMoAddressesAoLevel8-25.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addMoAddressesAoLevel8AoLevel25() {
        AreaType areaType = new AreaType();
        areaType.setCode(199L);

        List<Long> moAddresses = algorithms.searchServiceDistrictMOByAddressV33(areaType, 10L);

        assertEquals(1, moAddresses.size());
        assertEquals(20L, moAddresses.get(0).longValue());
    }

    @Test
    @Sql(scripts = {"/sql/addMoAddressesAoLevel8-2.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addMoAddressesAoLevel8AoLevel2() {
        AreaType areaType = new AreaType();
        areaType.setCode(199L);

        List<Long> moAddresses = algorithms.searchServiceDistrictMOByAddressV33(areaType, 10L);

        assertEquals(1, moAddresses.size());
        assertEquals(20L, moAddresses.get(0).longValue());
    }

    @Test
    @Sql(scripts = {"/sql/addMoAddressesAoLevel7-7.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addMoAddressesAoLevel7AoLevel7() {
        AreaType areaType = new AreaType();
        areaType.setCode(199L);

        List<Long> moAddresses = algorithms.searchServiceDistrictMOByAddressV33(areaType, 10L);

        assertEquals(1, moAddresses.size());
        assertEquals(20L, moAddresses.get(0).longValue());
    }

    @Test
    @Sql(scripts = {"/sql/addMoAddressesAoLevel7-65.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addMoAddressesAoLevel7AoLevel65() {
        AreaType areaType = new AreaType();
        areaType.setCode(199L);

        List<Long> moAddresses = algorithms.searchServiceDistrictMOByAddressV33(areaType, 30L);

        assertEquals(1, moAddresses.size());
        assertEquals(20L, moAddresses.get(0).longValue());
    }

    @Test
    @Sql(scripts = {"/sql/addMoAddressesAoLevel7-6.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addMoAddressesAoLevel7AoLevel6() {
        AreaType areaType = new AreaType();
        areaType.setCode(199L);

        List<Long> moAddresses = algorithms.searchServiceDistrictMOByAddressV33(areaType, 999L);

        assertEquals(1, moAddresses.size());
        assertEquals(20L, moAddresses.get(0).longValue());
    }

    @Test
    @Sql(scripts = {"/sql/addMoAddressesAoLevel7-4.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addMoAddressesAoLevel7AoLevel4() {
        AreaType areaType = new AreaType();
        areaType.setCode(199L);

        List<Long> moAddresses = algorithms.searchServiceDistrictMOByAddressV33(areaType, 50L);

        assertEquals(1, moAddresses.size());
        assertEquals(20L, moAddresses.get(0).longValue());
    }

    @Test
    @Sql(scripts = {"/sql/addMoAddressesAoLevel7-25.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addMoAddressesAoLevel7AoLevel25() {
        AreaType areaType = new AreaType();
        areaType.setCode(199L);

        List<Long> moAddresses = algorithms.searchServiceDistrictMOByAddressV33(areaType, 70L);

        assertEquals(1, moAddresses.size());
        assertEquals(20L, moAddresses.get(0).longValue());
    }

    @Test
    @Sql(scripts = {"/sql/addMoAddressesAoLevel7-2.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addMoAddressesAoLevel7AoLevel2() {
        AreaType areaType = new AreaType();
        areaType.setCode(199L);

        List<Long> moAddresses = algorithms.searchServiceDistrictMOByAddressV33(areaType, 80L);

        assertEquals(1, moAddresses.size());
        assertEquals(20L, moAddresses.get(0).longValue());
    }

    @Test
    @Sql(scripts = {"/sql/addMoAddressesAoLevel65-65.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addMoAddressesAoLevel65AoLevel65() {
        AreaType areaType = new AreaType();
        areaType.setCode(199L);

        List<Long> moAddresses = algorithms.searchServiceDistrictMOByAddressV33(areaType, 30L);

        assertEquals(1, moAddresses.size());
        assertEquals(20L, moAddresses.get(0).longValue());
    }

    @Test
    @Sql(scripts = {"/sql/addMoAddressesAoLevel65-6.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addMoAddressesAoLevel65AoLevel6() {
        AreaType areaType = new AreaType();
        areaType.setCode(199L);

        List<Long> moAddresses = algorithms.searchServiceDistrictMOByAddressV33(areaType, 40L);

        assertEquals(1, moAddresses.size());
        assertEquals(20L, moAddresses.get(0).longValue());
    }

    @Test
    @Sql(scripts = {"/sql/addMoAddressesAoLevel65-4.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addMoAddressesAoLevel65AoLevel4() {
        AreaType areaType = new AreaType();
        areaType.setCode(199L);

        List<Long> moAddresses = algorithms.searchServiceDistrictMOByAddressV33(areaType, 50L);

        assertEquals(1, moAddresses.size());
        assertEquals(20L, moAddresses.get(0).longValue());
    }

    @Test
    @Sql(scripts = {"/sql/addMoAddressesAoLevel65-25.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addMoAddressesAoLevel65AoLevel25() {
        AreaType areaType = new AreaType();
        areaType.setCode(199L);

        List<Long> moAddresses = algorithms.searchServiceDistrictMOByAddressV33(areaType, 60L);

        assertEquals(1, moAddresses.size());
        assertEquals(20L, moAddresses.get(0).longValue());
    }

    @Test
    @Sql(scripts = {"/sql/addMoAddressesAoLevel65-2.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addMoAddressesAoLevel65AoLevel2() {
        AreaType areaType = new AreaType();
        areaType.setCode(199L);

        List<Long> moAddresses = algorithms.searchServiceDistrictMOByAddressV33(areaType, 70L);

        assertEquals(1, moAddresses.size());
        assertEquals(20L, moAddresses.get(0).longValue());
    }

    @Test
    @Sql(scripts = {"/sql/addMoAddressesAoLevel6-6.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addMoAddressesAoLevel6AoLevel6() {
        AreaType areaType = new AreaType();
        areaType.setCode(199L);

        List<Long> moAddresses = algorithms.searchServiceDistrictMOByAddressV33(areaType, 30L);

        assertEquals(1, moAddresses.size());
        assertEquals(20L, moAddresses.get(0).longValue());
    }

    @Test
    @Sql(scripts = {"/sql/addMoAddressesAoLevel6-4.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addMoAddressesAoLevel6AoLevel4() {
        AreaType areaType = new AreaType();
        areaType.setCode(199L);

        List<Long> moAddresses = algorithms.searchServiceDistrictMOByAddressV33(areaType, 40L);

        assertEquals(1, moAddresses.size());
        assertEquals(20L, moAddresses.get(0).longValue());
    }

    @Test
    @Sql(scripts = {"/sql/addMoAddressesAoLevel6-25.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addMoAddressesAoLevel6AoLevel25() {
        AreaType areaType = new AreaType();
        areaType.setCode(199L);

        List<Long> moAddresses = algorithms.searchServiceDistrictMOByAddressV33(areaType, 50L);

        assertEquals(1, moAddresses.size());
        assertEquals(20L, moAddresses.get(0).longValue());
    }

    @Test
    @Sql(scripts = {"/sql/addMoAddressesAoLevel6-2.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addMoAddressesAoLevel6AoLevel2() {
        AreaType areaType = new AreaType();
        areaType.setCode(199L);

        List<Long> moAddresses = algorithms.searchServiceDistrictMOByAddressV33(areaType, 60L);

        assertEquals(1, moAddresses.size());
        assertEquals(20L, moAddresses.get(0).longValue());
    }

    @Test
    @Sql(scripts = {"/sql/addMoAddressesAoLevel4-4.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addMoAddressesAoLevel4AoLevel4() {
        AreaType areaType = new AreaType();
        areaType.setCode(199L);

        List<Long> moAddresses = algorithms.searchServiceDistrictMOByAddressV33(areaType, 40L);

        assertEquals(1, moAddresses.size());
        assertEquals(20L, moAddresses.get(0).longValue());
    }

    @Test
    @Sql(scripts = {"/sql/addMoAddressesAoLevel4-25.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addMoAddressesAoLevel4AoLevel25() {
        AreaType areaType = new AreaType();
        areaType.setCode(199L);

        List<Long> moAddresses = algorithms.searchServiceDistrictMOByAddressV33(areaType, 50L);

        assertEquals(1, moAddresses.size());
        assertEquals(20L, moAddresses.get(0).longValue());
    }

    @Test
    @Sql(scripts = {"/sql/addMoAddressesAoLevel4-2.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addMoAddressesAoLevel4AoLevel2() {
        AreaType areaType = new AreaType();
        areaType.setCode(199L);

        List<Long> moAddresses = algorithms.searchServiceDistrictMOByAddressV33(areaType, 60L);

        assertEquals(1, moAddresses.size());
        assertEquals(20L, moAddresses.get(0).longValue());
    }

    @Test
    @Sql(scripts = {"/sql/addMoAddressesAoLevel25-25.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addMoAddressesAoLevel25AoLevel25() {
        AreaType areaType = new AreaType();
        areaType.setCode(199L);

        List<Long> moAddresses = algorithms.searchServiceDistrictMOByAddressV33(areaType, 50L);

        assertEquals(1, moAddresses.size());
        assertEquals(20L, moAddresses.get(0).longValue());
    }

    @Test
    @Sql(scripts = {"/sql/addMoAddressesAoLevel25-2.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addMoAddressesAoLevel25AoLevel2() {
        AreaType areaType = new AreaType();
        areaType.setCode(199L);

        List<Long> moAddresses = algorithms.searchServiceDistrictMOByAddressV33(areaType, 60L);

        assertEquals(1, moAddresses.size());
        assertEquals(20L, moAddresses.get(0).longValue());
    }

    @Test
    @Sql(scripts = {"/sql/addMoAddressesAoLevel2-2.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addMoAddressesAoLevel2AoLevel2() {
        AreaType areaType = new AreaType();
        areaType.setCode(199L);

        List<Long> moAddresses = algorithms.searchServiceDistrictMOByAddressV33(areaType, 60L);

        assertEquals(1, moAddresses.size());
        assertEquals(20L, moAddresses.get(0).longValue());
    }

    // -------------------------------------------------------------------------------- //

    @Test
    @Sql(scripts = {"/sql/addMoAddressesAoLevel8ThenLevel8.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addMoAddressesAoLevel8ThenLevel8() {
        AreaType areaType = new AreaType();
        areaType.setCode(199L);

        for (long i = 20; i <= 80; i += 10) {
            List<Long> moAddresses = algorithms.searchServiceDistrictMOByAddressV33(areaType, i);

            assertEquals(1, moAddresses.size());
            assertEquals(20L, moAddresses.get(0).longValue());
        }
    }

    @Test
    @Sql(scripts = {"/sql/addMoAddressesAoLevel8ThenLevel7.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addMoAddressesAoLevel8ThenLevel7() {
        AreaType areaType = new AreaType();
        areaType.setCode(199L);

        for (long i = 20; i <= 80; i += 10) {
            List<Long> moAddresses = algorithms.searchServiceDistrictMOByAddressV33(areaType, i);

            assertEquals(1, moAddresses.size());
            assertEquals(20L, moAddresses.get(0).longValue());
        }
    }

    @Test
    @Sql(scripts = {"/sql/addMoAddressesAoLevel8AoThenLevel65.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addMoAddressesAoLevel8AoThenLevel65() {
        AreaType areaType = new AreaType();
        areaType.setCode(199L);

        for (long i = 20; i <= 80; i += 10) {
            List<Long> moAddresses = algorithms.searchServiceDistrictMOByAddressV33(areaType, 10L);

            assertEquals(1, moAddresses.size());
            assertEquals(20L, moAddresses.get(0).longValue());
        }
    }

    @Test
    @Sql(scripts = {"/sql/addMoAddressesAoLevel8ThenLevel6.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addMoAddressesAoLevel8ThenLevel6() {
        AreaType areaType = new AreaType();
        areaType.setCode(199L);

        for (long i = 20; i <= 80; i += 10) {
            List<Long> moAddresses = algorithms.searchServiceDistrictMOByAddressV33(areaType, 10L);

            assertEquals(1, moAddresses.size());
            assertEquals(20L, moAddresses.get(0).longValue());
        }
    }

    @Test
    @Sql(scripts = {"/sql/addMoAddressesAoLevel8ThenLevel4.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addMoAddressesAoLevel8ThenLevel4() {
        AreaType areaType = new AreaType();
        areaType.setCode(199L);

        for (long i = 20; i <= 80; i += 10) {
            List<Long> moAddresses = algorithms.searchServiceDistrictMOByAddressV33(areaType, 10L);

            assertEquals(1, moAddresses.size());
            assertEquals(20L, moAddresses.get(0).longValue());
        }
    }

    @Test
    @Sql(scripts = {"/sql/addMoAddressesAoLevel8ThenLevel25.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addMoAddressesAoLevel8ThenLevel25() {
        AreaType areaType = new AreaType();
        areaType.setCode(199L);

        for (long i = 20; i <= 80; i += 10) {
            List<Long> moAddresses = algorithms.searchServiceDistrictMOByAddressV33(areaType, 10L);

            assertEquals(1, moAddresses.size());
            assertEquals(20L, moAddresses.get(0).longValue());
        }
    }

    @Test
    @Sql(scripts = {"/sql/addMoAddressesAoLevel8ThenLevel2.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addMoAddressesAoLevel8ThenLevel2() {
        AreaType areaType = new AreaType();
        areaType.setCode(199L);

        for (long i = 20; i <= 80; i += 10) {
            List<Long> moAddresses = algorithms.searchServiceDistrictMOByAddressV33(areaType, 10L);

            assertEquals(1, moAddresses.size());
            assertEquals(20L, moAddresses.get(0).longValue());
        }
    }

    @Test
    @Sql(scripts = {"/sql/addMoAddressesAoLevel8-full.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addMoAddressesAoLevel8AoLevel2Full() {
        AreaType areaType = new AreaType();
        areaType.setCode(199L);

        List<Long> moAddresses = algorithms.searchServiceDistrictMOByAddressV33(areaType, 10L);

        List<Long> listId = moAddresses.stream().sorted().collect(Collectors.toList());

        assertEquals(1, listId.size());
        assertEquals(20, listId.get(0).longValue());
    }

    @Test
    @Sql(scripts = {"/sql/addMoAddressesAllMoAddresses.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addMoAddressesTestSearchAddressesUnknown() {
        AreaType areaType = new AreaType();
        areaType.setCode(999L);

        for (String aoLevel : new String[]{"8", "7", "65", "6", "4", "25", "2"}) {
            Addresses addresses = new Addresses();
            addresses.setId(-1L);
            addresses.setGlobalId(-1L);
            addresses.setStreetCode("16861");
            addresses.setAreaCodeOmkTe("02121");
            addresses.setPlanCode("00001");
            addresses.setPlaceCode("0001");
            addresses.setCityCode("0021");
            addresses.setRegionTeCode("08001");
            addresses.setAoLevel(aoLevel);
            List<Long> moAddresses = algorithms.searchServiceDistrictMOByAddressV33(areaType, 10L);
            assertTrue(moAddresses.isEmpty());
        }
    }

    @Test
    @Sql(scripts = {"/sql/addMoAddressesAllMoAddresses.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addMoAddressesTestSearchAreaTypeUnknown() {

        AreaType areaType = new AreaType();
        areaType.setCode(9991L);

        for (String aoLevel : new String[]{"8", "7", "65", "6", "4", "25", "2"}) {
            Addresses addresses = new Addresses();
            addresses.setId(-1L);
            addresses.setGlobalId(-1L);
            addresses.setStreetCode("1686");
            addresses.setAreaCodeOmkTe("0212");
            addresses.setPlanCode("0000");
            addresses.setPlaceCode("000");
            addresses.setCityCode("002");
            addresses.setRegionTeCode("0800");
            addresses.setAoLevel(aoLevel);
            List<Long> moAddresses = algorithms.searchServiceDistrictMOByAddressV33(areaType, 10L);
            assertTrue(moAddresses.isEmpty());
        }
    }

    @Test
    @Sql(scripts = {"/sql/addMoAddressesAllMoAddresses.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addMoAddressesTestSearchMoIdUnknown() {

        AreaType areaType = new AreaType();
        areaType.setCode(999L);

        for (String aoLevel : new String[]{"8", "7", "65", "6", "4", "25", "2"}) {
            Addresses addresses = new Addresses();
            addresses.setId(-1L);
            addresses.setGlobalId(-1L);
            addresses.setStreetCode("1686");
            addresses.setAreaCodeOmkTe("0212");
            addresses.setPlanCode("0000");
            addresses.setPlaceCode("000");
            addresses.setCityCode("002");
            addresses.setRegionTeCode("0800");
            addresses.setAoLevel(aoLevel);
            List<Long> moAddresses = algorithms.searchServiceDistrictMOByAddressV33(areaType, 101L);
            assertTrue(moAddresses.isEmpty());
        }
    }
}
