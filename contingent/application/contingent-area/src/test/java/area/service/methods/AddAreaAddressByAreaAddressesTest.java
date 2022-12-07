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
import moscow.ptnl.contingent.domain.area.entity.Addresses;
import moscow.ptnl.contingent.domain.area.entity.AreaAddress;
import moscow.ptnl.contingent.domain.area.entity.MoAddress;
import moscow.ptnl.contingent.error.Validation;
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
import java.util.List;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.*;

@ExtendWith(SpringExtension.class)
@ExtendWith(MockitoExtension.class)
@ContextConfiguration(classes = {PersistenceConfiguration.class, MockConfiguration.class})
@Transactional
public class AddAreaAddressByAreaAddressesTest extends service.BaseTest {

    @Autowired
    private Algorithms algorithms;

    @BeforeAll
    public static void init(@Qualifier("contingentDataSource") DataSource dataSource) throws LiquibaseException, SQLException {
        Database database = DatabaseFactory.getInstance().findCorrectDatabaseImplementation(new JdbcConnection(dataSource.getConnection()));
        Liquibase liquibase = new Liquibase("changelog/area/versions/master.xml", new FileSystemResourceAccessor("../database"), database);
        liquibase.update("");
    }

    // ----------getActiveMoAddressLevel8---------- //

    @Test // ищем МО адрес по совпадению globalId
    @Sql(scripts = {"/sql/areaAddresses8-1.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addAreaAddressesTest8() {
        Addresses addresses = new Addresses();
        addresses.setId(17061L);
        addresses.setGlobalId(78799044L);
        addresses.setAoLevel("8");

        AreaType areaType = new AreaType();
        areaType.setCode(999L);

        MoAddress moAddress = new MoAddress();
        moAddress.setId(1L);
        moAddress.setAddress(addresses);
        moAddress.setAreaType(areaType);
        moAddress.setMoId(-2202860707L);

        List<MoAddress> areaAddresses = algorithms.searchServiceDistrictMOByAddressV3(areaType, moAddress.getMoId(), addresses);

        assertEquals(1, areaAddresses.size());
        assertEquals(1L, areaAddresses.get(0).getId().longValue());
        assertEquals(78799044L, areaAddresses.get(0).getAddress().getGlobalId().longValue());
    }

    @Test // A_УУ_16 aoLevel 8-7
    @Sql(scripts = {"/sql/addAreaAddressesByAreaAddressesAoLevel8AoLevel7.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addAreaAddressesByAreaAddressesAoLevel8AoLevel8() {
        Addresses addresses  = new Addresses();
        addresses.setId(17062L);
        addresses.setGlobalId(1L);
        addresses.setStreetCode("1686");
        addresses.setAreaCodeOmkTe("0212");
        addresses.setPlaceCode("000");
        addresses.setPlanCode("0000");
        addresses.setCityCode("002");
        addresses.setAoLevel("8");

        AreaType areaType = new AreaType();
        areaType.setCode(999L);

        Validation validation = new Validation();

        List<AreaAddress> areaAddresses = algorithms.searchAreaByAddressV3(1L, areaType,  addresses, false, validation);

        assertEquals(1, areaAddresses.size());
        assertEquals(1L, areaAddresses.get(0).getId().longValue());
        assertEquals("1686", areaAddresses.get(0).getAddress().getStreetCode());
        assertEquals("0212", areaAddresses.get(0).getAddress().getAreaCodeOmkTe());
        assertEquals("000", areaAddresses.get(0).getAddress().getPlaceCode());
        assertEquals("0000", areaAddresses.get(0).getAddress().getPlanCode());
        assertEquals("002", areaAddresses.get(0).getAddress().getCityCode());
    }

    @Test // A_УУ_16 aoLevel 8-65
    @Sql(scripts = {"/sql/addAreaAddressesByAreaAddressesAoLevel8AoLevel65.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addAreaAddressesByAreaAddressesAoLevel8AoLevel65() {
        Addresses addresses  = new Addresses();
        addresses.setId(17063L);
        addresses.setGlobalId(1L);
        addresses.setAreaCodeOmkTe("0212");
        addresses.setPlaceCode("000");
        addresses.setPlanCode("0000");
        addresses.setCityCode("002");
        addresses.setAoLevel("8");

        AreaType areaType = new AreaType();
        areaType.setCode(999L);

        Validation validation = new Validation();

        List<AreaAddress> areaAddresses = algorithms.searchAreaByAddressV3(1L, areaType,  addresses, false, validation);

        assertEquals(1, areaAddresses.size());
        assertEquals(1L, areaAddresses.get(0).getId().longValue());
        assertEquals("0212", areaAddresses.get(0).getAddress().getAreaCodeOmkTe());
        assertEquals("000", areaAddresses.get(0).getAddress().getPlaceCode());
        assertEquals("0000", areaAddresses.get(0).getAddress().getPlanCode());
        assertEquals("002", areaAddresses.get(0).getAddress().getCityCode());
    }

    @Test // A_УУ_16 aoLevel 8-6
    @Sql(scripts = {"/sql/addAreaAddressesByAreaAddressesAoLevel8AoLevel6.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addAreaAddressesByAreaAddressesAoLevel8AoLevel6() {
        Addresses addresses  = new Addresses();
        addresses.setId(17064L);
        addresses.setGlobalId(1L);
        addresses.setAreaCodeOmkTe("0212");
        addresses.setPlaceCode("000");
        addresses.setCityCode("002");
        addresses.setAoLevel("8");

        AreaType areaType = new AreaType();
        areaType.setCode(999L);

        Validation validation = new Validation();

        List<AreaAddress> areaAddresses = algorithms.searchAreaByAddressV3(1L, areaType,  addresses, false, validation);

        assertEquals(1, areaAddresses.size());
        assertEquals(1L, areaAddresses.get(0).getId().longValue());
        assertEquals("0212", areaAddresses.get(0).getAddress().getAreaCodeOmkTe());
        assertEquals("000", areaAddresses.get(0).getAddress().getPlaceCode());
        assertEquals("002", areaAddresses.get(0).getAddress().getCityCode());
    }

    @Test // A_УУ_16 aoLevel 8-4
    @Sql(scripts = {"/sql/addAreaAddressesByAreaAddressesAoLevel8AoLevel4.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addAreaAddressesByAreaAddressesAoLevel8AoLevel4() {
        Addresses addresses  = new Addresses();
        addresses.setId(17065L);
        addresses.setGlobalId(1L);
        addresses.setAreaCodeOmkTe("0212");
        addresses.setCityCode("002");
        addresses.setAoLevel("8");

        AreaType areaType = new AreaType();
        areaType.setCode(999L);

        Validation validation = new Validation();

        List<AreaAddress> areaAddresses = algorithms.searchAreaByAddressV3(1L, areaType,  addresses, false, validation);

        assertEquals(1, areaAddresses.size());
        assertEquals(1L, areaAddresses.get(0).getId().longValue());
        assertEquals("0212", areaAddresses.get(0).getAddress().getAreaCodeOmkTe());
        assertEquals("002", areaAddresses.get(0).getAddress().getCityCode());
    }

    @Test // A_УУ_16 aoLevel 8-25
    @Sql(scripts = {"/sql/addAreaAddressesByAreaAddressesAoLevel8AoLevel25.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addAreaAddressesByAreaAddressesAoLevel8AoLevel25() {
        Addresses addresses  = new Addresses();
        addresses.setId(17066L);
        addresses.setGlobalId(1L);
        addresses.setAreaCodeOmkTe("0212");
        addresses.setAoLevel("8");

        AreaType areaType = new AreaType();
        areaType.setCode(999L);

        Validation validation = new Validation();

        List<AreaAddress> areaAddresses = algorithms.searchAreaByAddressV3(1L, areaType,  addresses, false, validation);

        assertEquals(1, areaAddresses.size());
        assertEquals(1L, areaAddresses.get(0).getId().longValue());
        assertEquals("0212", areaAddresses.get(0).getAddress().getAreaCodeOmkTe());
    }

    @Test // A_УУ_16 aoLevel 8-2
    @Sql(scripts = {"/sql/addAreaAddressesByMoAddressesAoLevel8AoLevel2.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addAreaAddressesByAreaAddressesAoLevel8AoLevel2() {
        Addresses addresses  = new Addresses();
        addresses.setId(17067L);
        addresses.setRegionTeCode("0800");
        addresses.setAoLevel("7");

        AreaType areaType = new AreaType();
        areaType.setCode(999L);

        MoAddress moAddress = new MoAddress();
        moAddress.setId(7L);
        moAddress.setMoId(-2202860707L);
        moAddress.setAddress(addresses);
        moAddress.setAreaType(areaType);

        List<MoAddress> areaAddresses = algorithms.searchServiceDistrictMOByAddressV3(areaType, moAddress.getMoId(), addresses);

        assertEquals(1, areaAddresses.size());
        assertEquals(7L, areaAddresses.get(0).getId().longValue());
        assertEquals("0800", areaAddresses.get(0).getAddress().getRegionTeCode());
    }

    // ----------getActiveMoAddressLevel7---------- //

    @Test // A_УУ_16 aoLevel 7-7
    @Sql(scripts = {"/sql/addAreaAddressesByAreaAddressesAoLevel7AoLevel7.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addAreaAddressesByAreaAddressesAoLevel7AoLevel7() {
        Addresses addresses  = new Addresses();
        addresses.setId(17062L);
        addresses.setGlobalId(1L);
        addresses.setStreetCode("1686");
        addresses.setAreaCodeOmkTe("0212");
        addresses.setPlaceCode("000");
        addresses.setPlanCode("0000");
        addresses.setCityCode("002");
        addresses.setAoLevel("7");

        AreaType areaType = new AreaType();
        areaType.setCode(999L);

        Validation validation = new Validation();

        List<AreaAddress> areaAddresses = algorithms.searchAreaByAddressV3(1L, areaType,  addresses, false, validation);

        assertEquals(1, areaAddresses.size());
        assertEquals(1L, areaAddresses.get(0).getId().longValue());
        assertEquals("1686", areaAddresses.get(0).getAddress().getStreetCode());
        assertEquals("0212", areaAddresses.get(0).getAddress().getAreaCodeOmkTe());
        assertEquals("000", areaAddresses.get(0).getAddress().getPlaceCode());
        assertEquals("0000", areaAddresses.get(0).getAddress().getPlanCode());
        assertEquals("002", areaAddresses.get(0).getAddress().getCityCode());
    }

    @Test // A_УУ_16 aoLevel 7-65
    @Sql(scripts = {"/sql/addAreaAddressesByAreaAddressesAoLevel7AoLevel65.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addAreaAddressesByAreaAddressesAoLevel7AoLevel65() {
        Addresses addresses  = new Addresses();
        addresses.setId(17071L);
        addresses.setGlobalId(1L);
        addresses.setAreaCodeOmkTe("0212");
        addresses.setPlaceCode("000");
        addresses.setPlanCode("0000");
        addresses.setCityCode("002");
        addresses.setAoLevel("7");

        AreaType areaType = new AreaType();
        areaType.setCode(999L);

        Validation validation = new Validation();

        List<AreaAddress> areaAddresses = algorithms.searchAreaByAddressV3(1L, areaType,  addresses, false, validation);

        assertEquals(1, areaAddresses.size());
        assertEquals(1L, areaAddresses.get(0).getId().longValue());
        assertEquals("0212", areaAddresses.get(0).getAddress().getAreaCodeOmkTe());
        assertEquals("000", areaAddresses.get(0).getAddress().getPlaceCode());
        assertEquals("0000", areaAddresses.get(0).getAddress().getPlanCode());
        assertEquals("002", areaAddresses.get(0).getAddress().getCityCode());
    }

    @Test // A_УУ_16 aoLevel 7-6
    @Sql(scripts = {"/sql/addAreaAddressesByAreaAddressesAoLevel7AoLevel6.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addAreaAddressesByAreaAddressesAoLevel7AoLevel6() {
        Addresses addresses  = new Addresses();
        addresses.setId(17072L);
        addresses.setGlobalId(1L);
        addresses.setAreaCodeOmkTe("0212");
        addresses.setPlaceCode("000");
        addresses.setCityCode("002");
        addresses.setAoLevel("7");

        AreaType areaType = new AreaType();
        areaType.setCode(999L);

        Validation validation = new Validation();

        List<AreaAddress> areaAddresses = algorithms.searchAreaByAddressV3(1L, areaType,  addresses, false, validation);

        assertEquals(1, areaAddresses.size());
        assertEquals(1L, areaAddresses.get(0).getId().longValue());
        assertEquals("0212", areaAddresses.get(0).getAddress().getAreaCodeOmkTe());
        assertEquals("000", areaAddresses.get(0).getAddress().getPlaceCode());
        assertEquals("002", areaAddresses.get(0).getAddress().getCityCode());
    }

    @Test // A_УУ_16 aoLevel 7-4
    @Sql(scripts = {"/sql/addAreaAddressesByAreaAddressesAoLevel7AoLevel4.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addAreaAddressesByAreaAddressesAoLevel7AoLevel4() {
        Addresses addresses  = new Addresses();
        addresses.setId(17073L);
        addresses.setGlobalId(1L);
        addresses.setAreaCodeOmkTe("0212");
        addresses.setCityCode("002");
        addresses.setAoLevel("7");

        AreaType areaType = new AreaType();
        areaType.setCode(999L);

        Validation validation = new Validation();

        List<AreaAddress> areaAddresses = algorithms.searchAreaByAddressV3(1L, areaType,  addresses, false, validation);

        assertEquals(1, areaAddresses.size());
        assertEquals(1L, areaAddresses.get(0).getId().longValue());
        assertEquals("0212", areaAddresses.get(0).getAddress().getAreaCodeOmkTe());
        assertEquals("002", areaAddresses.get(0).getAddress().getCityCode());
    }

    @Test // A_УУ_16 aoLevel 7-25
    @Sql(scripts = {"/sql/addAreaAddressesByAreaAddressesAoLevel7AoLevel25.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addAreaAddressesByAreaAddressesAoLevel7AoLevel25() {
        Addresses addresses  = new Addresses();
        addresses.setId(17074L);
        addresses.setGlobalId(1L);
        addresses.setAreaCodeOmkTe("0212");
        addresses.setAoLevel("7");

        AreaType areaType = new AreaType();
        areaType.setCode(999L);

        Validation validation = new Validation();

        List<AreaAddress> areaAddresses = algorithms.searchAreaByAddressV3(1L, areaType,  addresses, false, validation);

        assertEquals(1, areaAddresses.size());
        assertEquals(1L, areaAddresses.get(0).getId().longValue());
        assertEquals("0212", areaAddresses.get(0).getAddress().getAreaCodeOmkTe());
    }

    @Test // A_УУ_16 aoLevel 7-2
    @Sql(scripts = {"/sql/addAreaAddressesByAreaAddressesAoLevel7AoLevel2.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addAreaAddressesByAreaAddressesAoLevel7AoLevel2() {
        Addresses addresses  = new Addresses();
        addresses.setId(17067L);
        addresses.setRegionTeCode("0800");
        addresses.setAoLevel("7");

        AreaType areaType = new AreaType();
        areaType.setCode(999L);

        MoAddress moAddress = new MoAddress();
        moAddress.setId(6L);
        moAddress.setMoId(-2202860706L);
        moAddress.setAddress(addresses);
        moAddress.setAreaType(areaType);

        List<MoAddress> areaAddresses = algorithms.searchServiceDistrictMOByAddressV3(areaType, moAddress.getMoId(), addresses);

        assertEquals(1, areaAddresses.size());
        assertEquals(6L, areaAddresses.get(0).getId().longValue());
    }

    //  ----------getActiveMoAddressLevel65---------- //

    @Test // A_УУ_16 aoLevel 65-65
    @Sql(scripts = {"/sql/addAreaAddressesByAreaAddressesAoLevel65AoLevel65.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addAreaAddressesByAreaAddressesAoLevel65AoLevel65() {
        Addresses addresses  = new Addresses();
        addresses.setId(17071L);
        addresses.setGlobalId(1L);
        addresses.setAreaCodeOmkTe("0212");
        addresses.setPlaceCode("000");
        addresses.setPlanCode("0000");
        addresses.setCityCode("002");
        addresses.setAoLevel("7");

        AreaType areaType = new AreaType();
        areaType.setCode(999L);

        Validation validation = new Validation();

        List<AreaAddress> areaAddresses = algorithms.searchAreaByAddressV3(1L, areaType,  addresses, false, validation);

        assertEquals(1, areaAddresses.size());
        assertEquals(1L, areaAddresses.get(0).getId().longValue());
        assertEquals("0212", areaAddresses.get(0).getAddress().getAreaCodeOmkTe());
        assertEquals("000", areaAddresses.get(0).getAddress().getPlaceCode());
        assertEquals("0000", areaAddresses.get(0).getAddress().getPlanCode());
        assertEquals("002", areaAddresses.get(0).getAddress().getCityCode());
    }

    @Test // A_УУ_16 aoLevel 65-6
    @Sql(scripts = {"/sql/addAreaAddressesByAreaAddressesAoLevel65AoLevel6.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addAreaAddressesByAreaAddressesAoLevel65AoLevel6() {
        Addresses addresses  = new Addresses();
        addresses.setId(17072L);
        addresses.setGlobalId(1L);
        addresses.setAreaCodeOmkTe("0212");
        addresses.setPlaceCode("000");
        addresses.setCityCode("002");
        addresses.setAoLevel("7");

        AreaType areaType = new AreaType();
        areaType.setCode(999L);

        Validation validation = new Validation();

        List<AreaAddress> areaAddresses = algorithms.searchAreaByAddressV3(1L, areaType,  addresses, false, validation);

        assertEquals(1, areaAddresses.size());
        assertEquals(1L, areaAddresses.get(0).getId().longValue());
        assertEquals("0212", areaAddresses.get(0).getAddress().getAreaCodeOmkTe());
        assertEquals("000", areaAddresses.get(0).getAddress().getPlaceCode());
        assertEquals("002", areaAddresses.get(0).getAddress().getCityCode());
    }

    @Test // A_УУ_16 aoLevel 65-4
    @Sql(scripts = {"/sql/addAreaAddressesByAreaAddressesAoLevel65AoLevel4.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addAreaAddressesByAreaAddressesAoLevel65AoLevel4() {
        Addresses addresses  = new Addresses();
        addresses.setId(17073L);
        addresses.setGlobalId(1L);
        addresses.setAreaCodeOmkTe("0212");
        addresses.setCityCode("002");
        addresses.setAoLevel("7");

        AreaType areaType = new AreaType();
        areaType.setCode(999L);

        Validation validation = new Validation();

        List<AreaAddress> areaAddresses = algorithms.searchAreaByAddressV3(1L, areaType,  addresses, false, validation);

        assertEquals(1, areaAddresses.size());
        assertEquals(1L, areaAddresses.get(0).getId().longValue());
        assertEquals("0212", areaAddresses.get(0).getAddress().getAreaCodeOmkTe());
        assertEquals("002", areaAddresses.get(0).getAddress().getCityCode());
    }

    @Test // A_УУ_16 aoLevel 65-25
    @Sql(scripts = {"/sql/addAreaAddressesByAreaAddressesAoLevel65AoLevel25.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addAreaAddressesByAreaAddressesAoLevel65AoLevel25() {
        Addresses addresses  = new Addresses();
        addresses.setId(17074L);
        addresses.setGlobalId(1L);
        addresses.setAreaCodeOmkTe("0212");
        addresses.setAoLevel("7");

        AreaType areaType = new AreaType();
        areaType.setCode(999L);

        Validation validation = new Validation();

        List<AreaAddress> areaAddresses = algorithms.searchAreaByAddressV3(1L, areaType,  addresses, false, validation);

        assertEquals(1, areaAddresses.size());
        assertEquals(1L, areaAddresses.get(0).getId().longValue());
        assertEquals("0212", areaAddresses.get(0).getAddress().getAreaCodeOmkTe());
    }

    @Test // A_УУ_16 aoLevel 65-2
    @Sql(scripts = {"/sql/addAreaAddressesByAreaAddressesAoLevel65AoLevel2.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addAreaAddressesByAreaAddressesAoLevel65AoLevel2() {
        Addresses addresses  = new Addresses();
        addresses.setId(17075L);
        addresses.setRegionTeCode("0800");
        addresses.setAoLevel("7");

        AreaType areaType = new AreaType();
        areaType.setCode(999L);

        MoAddress moAddress = new MoAddress();
        moAddress.setId(6L);
        moAddress.setMoId(-2202860706L);
        moAddress.setAddress(addresses);
        moAddress.setAreaType(areaType);

        List<MoAddress> areaAddresses = algorithms.searchServiceDistrictMOByAddressV3(areaType, moAddress.getMoId(), addresses);

        assertEquals(1, areaAddresses.size());
        assertEquals("0800", areaAddresses.get(0).getAddress().getRegionTeCode());
        assertEquals(6L, areaAddresses.get(0).getId().longValue());
    }

    //  ----------getActiveMoAddressLevel6---------- //

    @Test // A_УУ_16 aoLevel 6-6
    @Sql(scripts = {"/sql/addAreaAddressesByAreaAddressesAoLevel6AoLevel6.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addAreaAddressesByAreaAddressesAoLevel6AoLevel6() {
        Addresses addresses  = new Addresses();
        addresses.setId(17072L);
        addresses.setGlobalId(1L);
        addresses.setAreaCodeOmkTe("0212");
        addresses.setPlaceCode("000");
        addresses.setCityCode("002");
        addresses.setAoLevel("7");

        AreaType areaType = new AreaType();
        areaType.setCode(999L);

        Validation validation = new Validation();

        List<AreaAddress> areaAddresses = algorithms.searchAreaByAddressV3(1L, areaType,  addresses, false, validation);

        assertEquals(1, areaAddresses.size());
        assertEquals(1L, areaAddresses.get(0).getId().longValue());
        assertEquals("0212", areaAddresses.get(0).getAddress().getAreaCodeOmkTe());
        assertEquals("000", areaAddresses.get(0).getAddress().getPlaceCode());
        assertEquals("002", areaAddresses.get(0).getAddress().getCityCode());
    }

    @Test // A_УУ_16 aoLevel 6-4
    @Sql(scripts = {"/sql/addAreaAddressesByAreaAddressesAoLevel4AoLevel4.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addAreaAddressesByAreaAddressesAoLevel6AoLevel4() {
        Addresses addresses  = new Addresses();
        addresses.setId(17073L);
        addresses.setGlobalId(1L);
        addresses.setAreaCodeOmkTe("0212");
        addresses.setCityCode("002");
        addresses.setAoLevel("7");

        AreaType areaType = new AreaType();
        areaType.setCode(999L);

        Validation validation = new Validation();

        List<AreaAddress> areaAddresses = algorithms.searchAreaByAddressV3(1L, areaType,  addresses, false, validation);

        assertEquals(1, areaAddresses.size());
        assertEquals(1L, areaAddresses.get(0).getId().longValue());
        assertEquals("0212", areaAddresses.get(0).getAddress().getAreaCodeOmkTe());
        assertEquals("002", areaAddresses.get(0).getAddress().getCityCode());
    }

    @Test // A_УУ_16 aoLevel 6-25
    @Sql(scripts = {"/sql/addAreaAddressesByAreaAddressesAoLevel6AoLevel25.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addAreaAddressesByAreaAddressesAoLevel6AoLevel25() {
        Addresses addresses  = new Addresses();
        addresses.setId(17074L);
        addresses.setGlobalId(1L);
        addresses.setAreaCodeOmkTe("0212");
        addresses.setAoLevel("7");

        AreaType areaType = new AreaType();
        areaType.setCode(999L);

        Validation validation = new Validation();

        List<AreaAddress> areaAddresses = algorithms.searchAreaByAddressV3(1L, areaType,  addresses, false, validation);

        assertEquals(1, areaAddresses.size());
        assertEquals(1L, areaAddresses.get(0).getId().longValue());
        assertEquals("0212", areaAddresses.get(0).getAddress().getAreaCodeOmkTe());
    }

    @Test // A_УУ_16 aoLevel 6-2
    @Sql(scripts = {"/sql/addAreaAddressesByAreaAddressesAoLevel6AoLevel2.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addAreaAddressesByAreaAddressesAoLevel6AoLevel2() {
        Addresses addresses  = new Addresses();
        addresses.setId(17075L);
        addresses.setRegionTeCode("0800");
        addresses.setAoLevel("7");

        AreaType areaType = new AreaType();
        areaType.setCode(999L);

        MoAddress moAddress = new MoAddress();
        moAddress.setId(6L);
        moAddress.setMoId(-2202860706L);
        moAddress.setAddress(addresses);
        moAddress.setAreaType(areaType);

        List<MoAddress> areaAddresses = algorithms.searchServiceDistrictMOByAddressV3(areaType, moAddress.getMoId(), addresses);

        assertEquals(1, areaAddresses.size());
        assertEquals("0800", areaAddresses.get(0).getAddress().getRegionTeCode());
        assertEquals(6L, areaAddresses.get(0).getId().longValue());
    }

    //  ----------getActiveMoAddressLevel4---------- //

    @Test // A_УУ_16 aoLevel 4-4
    @Sql(scripts = {"/sql/addAreaAddressesByAreaAddressesAoLevel4AoLevel4.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addAreaAddressesByAreaAddressesAoLevel4AoLevel4() {
        Addresses addresses  = new Addresses();
        addresses.setId(17073L);
        addresses.setGlobalId(1L);
        addresses.setAreaCodeOmkTe("0212");
        addresses.setCityCode("002");
        addresses.setAoLevel("7");

        AreaType areaType = new AreaType();
        areaType.setCode(999L);

        Validation validation = new Validation();

        List<AreaAddress> areaAddresses = algorithms.searchAreaByAddressV3(1L, areaType,  addresses, false, validation);

        assertEquals(1, areaAddresses.size());
        assertEquals(1L, areaAddresses.get(0).getId().longValue());
        assertEquals("0212", areaAddresses.get(0).getAddress().getAreaCodeOmkTe());
        assertEquals("002", areaAddresses.get(0).getAddress().getCityCode());
    }

    @Test // A_УУ_16 aoLevel 4-25
    @Sql(scripts = {"/sql/addAreaAddressesByAreaAddressesAoLevel4AoLevel25.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addAreaAddressesByAreaAddressesAoLevel4AoLevel25() {
        Addresses addresses  = new Addresses();
        addresses.setId(17074L);
        addresses.setGlobalId(1L);
        addresses.setAreaCodeOmkTe("0212");
        addresses.setAoLevel("7");

        AreaType areaType = new AreaType();
        areaType.setCode(999L);

        Validation validation = new Validation();

        List<AreaAddress> areaAddresses = algorithms.searchAreaByAddressV3(1L, areaType,  addresses, false, validation);

        assertEquals(1, areaAddresses.size());
        assertEquals(1L, areaAddresses.get(0).getId().longValue());
        assertEquals("0212", areaAddresses.get(0).getAddress().getAreaCodeOmkTe());
    }

    @Test // A_УУ_16 aoLevel 4-2
    @Sql(scripts = {"/sql/addAreaAddressesByAreaAddressesAoLevel4AoLevel2.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addAreaAddressesByAreaAddressesAoLevel4AoLevel2() {
        Addresses addresses  = new Addresses();
        addresses.setId(17075L);
        addresses.setRegionTeCode("0800");
        addresses.setAoLevel("7");

        AreaType areaType = new AreaType();
        areaType.setCode(999L);

        MoAddress moAddress = new MoAddress();
        moAddress.setId(6L);
        moAddress.setMoId(-2202860706L);
        moAddress.setAddress(addresses);
        moAddress.setAreaType(areaType);

        List<MoAddress> areaAddresses = algorithms.searchServiceDistrictMOByAddressV3(areaType, moAddress.getMoId(), addresses);

        assertEquals(1, areaAddresses.size());
        assertEquals("0800", areaAddresses.get(0).getAddress().getRegionTeCode());
        assertEquals(6L, areaAddresses.get(0).getId().longValue());
    }

    //  ----------getActiveMoAddressLevel25---------- //

    @Test // A_УУ_16 aoLevel 25-25
    @Sql(scripts = {"/sql/addAreaAddressesByAreaAddressesAoLevel25AoLevel25.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addAreaAddressesByAreaAddressesAoLevel25AoLevel25() {
        Addresses addresses  = new Addresses();
        addresses.setId(17074L);
        addresses.setGlobalId(1L);
        addresses.setAreaCodeOmkTe("0212");
        addresses.setAoLevel("7");

        AreaType areaType = new AreaType();
        areaType.setCode(999L);

        Validation validation = new Validation();

        List<AreaAddress> areaAddresses = algorithms.searchAreaByAddressV3(1L, areaType,  addresses, false, validation);

        assertEquals(1, areaAddresses.size());
        assertEquals(1L, areaAddresses.get(0).getId().longValue());
        assertEquals("0212", areaAddresses.get(0).getAddress().getAreaCodeOmkTe());
    }

    @Test // A_УУ_16 aoLevel 25-2
    @Sql(scripts = {"/sql/addAreaAddressesByAreaAddressesAoLevel25AoLevel2.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addAreaAddressesByAreaAddressesAoLevel25AoLevel2() {
        Addresses addresses  = new Addresses();
        addresses.setId(17075L);
        addresses.setRegionTeCode("0800");
        addresses.setAoLevel("7");

        AreaType areaType = new AreaType();
        areaType.setCode(999L);

        MoAddress moAddress = new MoAddress();
        moAddress.setId(6L);
        moAddress.setMoId(-2202860706L);
        moAddress.setAddress(addresses);
        moAddress.setAreaType(areaType);

        List<MoAddress> areaAddresses = algorithms.searchServiceDistrictMOByAddressV3(areaType, moAddress.getMoId(), addresses);

        assertEquals(1, areaAddresses.size());
        assertEquals("0800", areaAddresses.get(0).getAddress().getRegionTeCode());
        assertEquals(6L, areaAddresses.get(0).getId().longValue());
    }

    //  ----------getActiveMoAddressLevel2---------- //

    @Test // A_УУ_16 aoLevel 2-2
    @Sql(scripts = {"/sql/addAreaAddressesByAreaAddressesAoLevel2AoLevel2.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addAreaAddressesByAreaAddressesAoLevel2AoLevel2() {
        Addresses addresses  = new Addresses();
        addresses.setId(17075L);
        addresses.setRegionTeCode("0800");
        addresses.setAoLevel("7");

        AreaType areaType = new AreaType();
        areaType.setCode(999L);

        MoAddress moAddress = new MoAddress();
        moAddress.setId(6L);
        moAddress.setMoId(-2202860706L);
        moAddress.setAddress(addresses);
        moAddress.setAreaType(areaType);

        List<MoAddress> areaAddresses = algorithms.searchServiceDistrictMOByAddressV3(areaType, moAddress.getMoId(), addresses);

        assertEquals(1, areaAddresses.size());
        assertEquals("0800", areaAddresses.get(0).getAddress().getRegionTeCode());
        assertEquals(6L, areaAddresses.get(0).getId().longValue());
    }

    @Test
    @Sql(scripts = {"/sql/addAreaAddressesAllAreaAddresses.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addAreaAddressesTestLessThen8() {
        Addresses addresses = new Addresses();
        addresses.setId(-1L);
        addresses.setGlobalId(-1L);
        addresses.setStreetCode("1686");
        addresses.setAreaCodeOmkTe("0212");
        addresses.setPlanCode("0000");
        addresses.setPlaceCode("000");
        addresses.setCityCode("002");
        addresses.setRegionTeCode("0800");
        addresses.setAoLevel("8");

        AreaType areaType = new AreaType();
        areaType.setCode(999L);

        List<MoAddress> areaAddresses = algorithms.searchServiceDistrictMOByAddressV3(areaType, -2202860706L, addresses);
        List<Long> listMoAdressesId = areaAddresses.stream().map(item -> item.getId()).collect(Collectors.toList());
        assertEquals(6, listMoAdressesId.size());
        assertFalse(listMoAdressesId.contains(5L));
        for (long i = 6; i <= 11; i++) {
            assertTrue(listMoAdressesId.contains(i));
        }
    }

    @Test
    @Sql(scripts = {"/sql/addAreaAddressesAllAreaAddresses.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addAreaAddressesTestLessThen7() {
        Addresses addresses = new Addresses();
        addresses.setId(-1L);
        addresses.setGlobalId(-1L);
        addresses.setStreetCode("1686");
        addresses.setAreaCodeOmkTe("0212");
        addresses.setPlanCode("0000");
        addresses.setPlaceCode("000");
        addresses.setCityCode("002");
        addresses.setRegionTeCode("0800");
        addresses.setAoLevel("7");

        AreaType areaType = new AreaType();
        areaType.setCode(999L);

        List<MoAddress> areaAddresses = algorithms.searchServiceDistrictMOByAddressV3(areaType, -2202860706L, addresses);
        List<Long> listMoAdressesId = areaAddresses.stream().map(item -> item.getId()).collect(Collectors.toList());
        assertEquals(5, listMoAdressesId.size());
        assertFalse(listMoAdressesId.contains(5L));
        assertFalse(listMoAdressesId.contains(6L));
        for (long i = 7; i <= 11; i++) {
            assertTrue(listMoAdressesId.contains(i));
        }
    }

    @Test
    @Sql(scripts = {"/sql/addAreaAddressesAllAreaAddresses.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addAreaAddressesTestLessThen65() {
        Addresses addresses = new Addresses();
        addresses.setId(-1L);
        addresses.setGlobalId(-1L);
        addresses.setStreetCode("1686");
        addresses.setAreaCodeOmkTe("0212");
        addresses.setPlanCode("0000");
        addresses.setPlaceCode("000");
        addresses.setCityCode("002");
        addresses.setRegionTeCode("0800");
        addresses.setAoLevel("65");

        AreaType areaType = new AreaType();
        areaType.setCode(999L);

        List<MoAddress> areaAddresses = algorithms.searchServiceDistrictMOByAddressV3(areaType, -2202860706L, addresses);
        List<Long> listMoAdressesId = areaAddresses.stream().map(item -> item.getId()).collect(Collectors.toList());
        assertEquals(4, listMoAdressesId.size());
        assertFalse(listMoAdressesId.contains(5L));
        assertFalse(listMoAdressesId.contains(6L));
        assertFalse(listMoAdressesId.contains(7L));
        for (long i = 8; i <= 11; i++) {
            assertTrue(listMoAdressesId.contains(i));
        }
    }

    @Test
    @Sql(scripts = {"/sql/addAreaAddressesAllAreaAddresses.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addAreaAddressesTestLessThen6() {
        Addresses addresses = new Addresses();
        addresses.setId(-1L);
        addresses.setGlobalId(-1L);
        addresses.setStreetCode("1686");
        addresses.setAreaCodeOmkTe("0212");
        addresses.setPlanCode("0000");
        addresses.setPlaceCode("000");
        addresses.setCityCode("002");
        addresses.setRegionTeCode("0800");
        addresses.setAoLevel("6");

        AreaType areaType = new AreaType();
        areaType.setCode(999L);

        List<MoAddress> areaAddresses = algorithms.searchServiceDistrictMOByAddressV3(areaType, -2202860706L, addresses);
        List<Long> listMoAdressesId = areaAddresses.stream().map(item -> item.getId()).collect(Collectors.toList());
        assertEquals(3, listMoAdressesId.size());
        assertFalse(listMoAdressesId.contains(5L));
        assertFalse(listMoAdressesId.contains(6L));
        assertFalse(listMoAdressesId.contains(7L));
        assertFalse(listMoAdressesId.contains(8L));
        for (long i = 9; i <= 11; i++) {
            assertTrue(listMoAdressesId.contains(i));
        }
    }

    @Test
    @Sql(scripts = {"/sql/addAreaAddressesAllAreaAddresses.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addAreaAddressesTestLessThen4() {
        Addresses addresses = new Addresses();
        addresses.setId(-1L);
        addresses.setGlobalId(-1L);
        addresses.setStreetCode("1686");
        addresses.setAreaCodeOmkTe("0212");
        addresses.setPlanCode("0000");
        addresses.setPlaceCode("000");
        addresses.setCityCode("002");
        addresses.setRegionTeCode("0800");
        addresses.setAoLevel("4");

        AreaType areaType = new AreaType();
        areaType.setCode(999L);

        List<MoAddress> areaAddresses = algorithms.searchServiceDistrictMOByAddressV3(areaType, -2202860706L, addresses);
        List<Long> listMoAdressesId = areaAddresses.stream().map(item -> item.getId()).collect(Collectors.toList());
        assertEquals(2, listMoAdressesId.size());
        assertFalse(listMoAdressesId.contains(5L));
        assertFalse(listMoAdressesId.contains(6L));
        assertFalse(listMoAdressesId.contains(7L));
        assertFalse(listMoAdressesId.contains(8L));
        assertFalse(listMoAdressesId.contains(9L));
        for (long i = 10; i <= 11; i++) {
            assertTrue(listMoAdressesId.contains(i));
        }
    }

    @Test
    @Sql(scripts = {"/sql/addAreaAddressesAllAreaAddresses.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addAreaAddressesTestLessThen25() {
        Addresses addresses = new Addresses();
        addresses.setId(-1L);
        addresses.setGlobalId(-1L);
        addresses.setStreetCode("1686");
        addresses.setAreaCodeOmkTe("0212");
        addresses.setPlanCode("0000");
        addresses.setPlaceCode("000");
        addresses.setCityCode("002");
        addresses.setRegionTeCode("0800");
        addresses.setAoLevel("25");

        AreaType areaType = new AreaType();
        areaType.setCode(999L);

        List<MoAddress> areaAddresses = algorithms.searchServiceDistrictMOByAddressV3(areaType, -2202860706L, addresses);
        List<Long> listMoAdressesId = areaAddresses.stream().map(item -> item.getId()).collect(Collectors.toList());
        assertEquals(1, listMoAdressesId.size());
        assertFalse(listMoAdressesId.contains(5L));
        assertFalse(listMoAdressesId.contains(6L));
        assertFalse(listMoAdressesId.contains(7L));
        assertFalse(listMoAdressesId.contains(8L));
        assertFalse(listMoAdressesId.contains(9L));
        assertFalse(listMoAdressesId.contains(10L));
        for (long i = 11; i <= 11; i++) {
            assertTrue(listMoAdressesId.contains(i));
        }
    }

    @Test
    @Sql(scripts = {"/sql/addAreaAddressesAllAreaAddresses.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addAreaAddressesTestSearchAddressesUnknown() {
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
            List<MoAddress> areaAddresses = algorithms.searchServiceDistrictMOByAddressV3(areaType, -2202860706L, addresses);
            assertTrue(areaAddresses.isEmpty());
        }
    }

    @Test
    @Sql(scripts = {"/sql/addAreaAddressesAllAreaAddresses.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addAreaAddressesTestSearchAreaTypeUnknown() {
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
            List<MoAddress> areaAddresses = algorithms.searchServiceDistrictMOByAddressV3(areaType, -2202860706L, addresses);
            assertTrue(areaAddresses.isEmpty());
        }
    }

    @Test
    @Sql(scripts = {"/sql/addAreaAddressesAllAreaAddresses.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addAreaAddressesTestSearchMoIdUnknown() {

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
            List<MoAddress> areaAddresses = algorithms.searchServiceDistrictMOByAddressV3(areaType, -22028607061L, addresses);
            assertTrue(areaAddresses.isEmpty());
        }
    }
}
