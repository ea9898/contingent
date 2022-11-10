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
import moscow.ptnl.contingent.domain.area.model.area.AddressRegistry;
import moscow.ptnl.contingent.domain.area.model.area.Area;
import moscow.ptnl.contingent.domain.area.model.area.AreaOMKTE;
import moscow.ptnl.contingent.domain.area.model.area.Building;
import moscow.ptnl.contingent.domain.area.model.area.City;
import moscow.ptnl.contingent.domain.area.model.area.Names;
import moscow.ptnl.contingent.domain.area.model.area.Place;
import moscow.ptnl.contingent.domain.area.model.area.Plan;
import moscow.ptnl.contingent.domain.area.model.area.Region;
import moscow.ptnl.contingent.domain.area.model.area.RegionOMKTE;
import moscow.ptnl.contingent.domain.area.model.area.Street;
import moscow.ptnl.contingent.domain.area.repository.AddressesRepository;
import moscow.ptnl.contingent.error.ContingentException;
import moscow.ptnl.contingent.error.Validation;
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
import org.yaml.snakeyaml.constructor.Construct;
import ru.mos.emias.contingent2.core.v3.Address;

import javax.sql.DataSource;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.CountDownLatch;

import static org.junit.jupiter.api.Assertions.*;

@ExtendWith(SpringExtension.class)
@ExtendWith(MockitoExtension.class)
@ContextConfiguration(classes = {PersistenceConfiguration.class, MockConfiguration.class})
@Transactional
public class A_YY_6_Test {

    @Autowired
    private Algorithms algorithms;

    @Autowired
    private AddressesRepository addressesRepository;

    @BeforeAll
    public static void init(@Qualifier("contingentDataSource") DataSource dataSource) throws LiquibaseException, SQLException {
        Database database = DatabaseFactory.getInstance().findCorrectDatabaseImplementation(new JdbcConnection(dataSource.getConnection()));
        Liquibase liquibase = new Liquibase("changelog/area/versions/master.xml", new FileSystemResourceAccessor("../database"), database);
        liquibase.update("");
    }

    @Test // формато логический контроль шаг 1 когда находится адрес по глобал id
    @Sql(scripts = {"/sql/checkAddressFLKV3.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void checkAddressFLKV3FindAddressByGlobalId() {
        List<AddressRegistry> list = new ArrayList<>();
        AddressRegistry addressRegistry = new AddressRegistry();
        addressRegistry.setGlobalIdNsi(10L);

        list.add(addressRegistry);

        List<Addresses> addressesList = algorithms.checkAddressFLKV3(list, new Validation());

        assertNotNull(addressesList);
        assertEquals(1, addressesList.size());
        assertEquals(17062L, addressesList.get(0).getId().longValue());

    }

    @Test // формато логический контроль добавление нового адреса
    public void checkAddressFLKV3AddAddresses() {
        List<AddressRegistry> list = new ArrayList<>();

        Names regionType = new Names();
        regionType.setFull("regionType");
        regionType.setShort("region");

        Region region = new Region();
        region.setId("id");
        region.setName("name");
        region.setType(regionType);
        region.setCode("002");
        region.setFiasGuid("fiasGudi");

        Names regionOMKTEType = new Names();
        regionOMKTEType.setFull("regionOMKTEType");
        regionOMKTEType.setShort("regionOMKTE");

        RegionOMKTE regionOMKTE = new RegionOMKTE();
        regionOMKTE.setId("id");
        regionOMKTE.setCode("001");
        regionOMKTE.setName("name");
        regionOMKTE.setType(regionOMKTEType);
        regionOMKTE.setShortName("shortName");

        Names areaOMKTEType = new Names();
        areaOMKTEType.setFull("areaOMKTEType");
        areaOMKTEType.setShort("areaOMKTE");

        AreaOMKTE areaOMKTE = new AreaOMKTE();
        areaOMKTE.setId("id");
        areaOMKTE.setCode("000");
        areaOMKTE.setName("name");
        areaOMKTE.setType(areaOMKTEType);

        Names areaType = new Names();
        areaType.setFull("areaType");
        areaType.setShort("area");

        Area area = new Area();
        area.setCode("code");
        area.setCodeBTI("codeBTI");
        area.setName("name");
        area.setType(areaType);
        area.setId("id");
        area.setCodeOMKTE("codeOMKTE");
        area.setFiasGuid("fiasGUID");

        Names cityType = new Names();
        cityType.setFull("cityType");
        cityType.setShort("city");

        City city = new City();
        city.setCode("code");
        city.setCodeBTI("codeBTI");
        city.setName("name");
        city.setType(cityType);
        city.setCodeOMKTM("codeOMKTE");
        city.setId("id");

        Names placeType = new Names();
        placeType.setFull("placeType");
        placeType.setShort("place");

        Place place = new Place();
        place.setCode("code");
        place.setCodeBTI("codeBTI");
        place.setName("name");
        place.setType(placeType);
        place.setCodeOMKTM("codeOMKTE");
        place.setId("id");
        place.setFiasGuid("fiasGUID");

        Names planType = new Names();
        planType.setFull("planType");
        planType.setShort("plan");

        Plan plan = new Plan();
        plan.setCode("code");
        plan.setCodeBTI("codeBTI");
        plan.setName("name");
        plan.setType(planType);
        plan.setFiasGuid("fiasGUID");
        plan.setId("id");

        Names streetType = new Names();
        streetType.setFull("streetType");
        streetType.setShort("street");

        Street street = new Street();
        street.setCode("code");
        street.setCodeBTI("codeBTI");
        street.setType(streetType);
        street.setCodeOMKUM("codeOMKUM");
        street.setFiasGuid("fiasGUID");
        street.setId("id");
        street.setName("name");

        Names houseType = new Names();
        houseType.setFull("houseType");
        houseType.setShort("house");

        Building.House house = new Building.House();
        house.setType(houseType);
        house.setName("house");

        Names buildType = new Names();
        buildType.setFull("buildType");
        buildType.setShort("build");

        Building.Build build = new Building.Build();
        build.setType(buildType);
        build.setName("build");

        Names constructionType = new Names();
        constructionType.setFull("constructionType");
        constructionType.setShort("construction");

        Building.Construction construction = new Building.Construction();
        construction.setType(constructionType);
        construction.setName("construction");

        Building building = new Building();
        building.setHouse(house);
        building.setConstruction(construction);
        building.setBuild(build);
        building.setCadastralNumber("112233");
        building.setClazz("clazz");
        building.setEmergency(true);
        building.setFiasGuid("fiasGuid");
        building.setGeoData("geoData");
        building.setUnom(1L);
        building.setNumberOfStoreys(1L);
        building.setYearOfConstruction(1L);

        AddressRegistry addressRegistry = new AddressRegistry();
        addressRegistry.setGlobalIdNsi(10L);
        addressRegistry.setRegion(region);
        addressRegistry.setAddressString("addressString");
        addressRegistry.setFiasGuid("fiasGuid");
        addressRegistry.setCodePostal("codePostal");
        addressRegistry.setCodeKLADR("codeKLADR");
        addressRegistry.setNonActualAddress("nonActualAddress");
        addressRegistry.setAoLevel("8");
        addressRegistry.setAreaOMKTE(areaOMKTE);
        addressRegistry.setRegionOMKTE(regionOMKTE);
        addressRegistry.setArea(area);
        addressRegistry.setCity(city);
        addressRegistry.setPlace(place);
        addressRegistry.setPlan(plan);
        addressRegistry.setStreet(street);
        addressRegistry.setBuilding(building);

        list.add(addressRegistry);

        List<Addresses> addressesList = algorithms.checkAddressFLKV3(list, new Validation());

        assertNotNull(addressesList);
        assertEquals(1, addressesList.size());

    }
}
