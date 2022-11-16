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

import javax.sql.DataSource;
import java.sql.SQLException;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.Assertions.assertFalse;

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
        List<AddressRegistry> addressRegistries = new ArrayList<>();

        Names regionType = new Names();
        regionType.setFull("regionType");
        regionType.setShort("region");

        Region region = new Region();
        region.setId("1");
        region.setName("name");
        region.setType(regionType);
        region.setCode("002");
        region.setFiasGuid("fiasGudi");

        Names regionOMKTEType = new Names();
        regionOMKTEType.setFull("regionOMKTEType");
        regionOMKTEType.setShort("regionOMKTE");

        RegionOMKTE regionOMKTE = new RegionOMKTE();
        regionOMKTE.setId("1");
        regionOMKTE.setCode("001");
        regionOMKTE.setName("name");
        regionOMKTE.setType(regionOMKTEType);
        regionOMKTE.setShortName("shortName");

        Names areaOMKTEType = new Names();
        areaOMKTEType.setFull("areaOMKTEType");
        areaOMKTEType.setShort("areaOMKTE");

        AreaOMKTE areaOMKTE = new AreaOMKTE();
        areaOMKTE.setId("1");
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
        area.setId("1");
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
        city.setId("1");
        city.setFiasGuid("fiasGuid");

        Names placeType = new Names();
        placeType.setFull("placeType");
        placeType.setShort("place");

        Place place = new Place();
        place.setCode("code");
        place.setCodeBTI("codeBTI");
        place.setName("name");
        place.setType(placeType);
        place.setCodeOMKTM("codeOMKTE");
        place.setId("1");
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
        plan.setId("1");

        Names streetType = new Names();
        streetType.setFull("streetType");
        streetType.setShort("street");

        Street street = new Street();
        street.setCode("code");
        street.setCodeBTI("codeBTI");
        street.setType(streetType);
        street.setCodeOMKUM("codeOMKUM");
        street.setFiasGuid("fiasGUID");
        street.setId("1");
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

        addressRegistries.add(addressRegistry);

        Validation validation = new Validation();

        List<Addresses> addressesList = algorithms.checkAddressFLKV3(addressRegistries, validation);

        assertNotNull(addressesList);
        assertEquals(1, addressesList.size());
    }

    @Test
    // Система проверяет, что уровень адреса (aoLevel) принимает одно из допустимых значений, иначе возвращает ошибку С_УУ_107
    public void checkAddressFLKV3FindAoLevel() {
        List<AddressRegistry> list = new ArrayList<>();
        AddressRegistry addressRegistry = new AddressRegistry();
        addressRegistry.setGlobalIdNsi(10L);
        addressRegistry.setAoLevel("9");

        list.add(addressRegistry);
        Validation validation = new Validation();

        algorithms.checkAddressFLKV3(list, validation);
        assertFalse(validation.isSuccess());
        assertNotNull(validation.getMessages());
        assertEquals(1, validation.getMessages().size());
        assertEquals("UE072", validation.getMessages().get(0).getCode());
        assertEquals("Некорректный уровень адреса (9)", validation.getMessages().get(0).getMessage());
    }

    @Test // Система проверяет, что не передан уровень адреса (aoLevel), иначе возвращает ошибку С_УУ_110
    public void checkAddressFLKV3FindAoLevelNull() {
        List<AddressRegistry> list = new ArrayList<>();
        AddressRegistry addressRegistry = new AddressRegistry();
        addressRegistry.setGlobalIdNsi(10L);

        list.add(addressRegistry);
        Validation validation = new Validation();

        algorithms.checkAddressFLKV3(list, validation);
        assertFalse(validation.isSuccess());
        assertNotNull(validation.getMessages());
        assertEquals(1, validation.getMessages().size());
        assertEquals("UE075", validation.getMessages().get(0).getCode());
        assertEquals("Невозможно добавить адрес, т.к. не указан его уровень", validation.getMessages().get(0).getMessage());
    }

    @Test // aoLevel <> 1 и не параметр передан код округа (regionOMKTE/code)
    public void checkAddressFLKV3FindAoLevelNotOne() {
        List<AddressRegistry> list = new ArrayList<>();
        AddressRegistry addressRegistry = new AddressRegistry();
        addressRegistry.setGlobalIdNsi(10L);
        addressRegistry.setAoLevel("3"); // aoLevel <> 1

        list.add(addressRegistry);
        Validation validation = new Validation();

        algorithms.checkAddressFLKV3(list, validation);

        assertFalse(validation.isSuccess());
        assertNotNull(validation.getMessages());
        assertEquals(1, validation.getMessages().size());
        assertEquals("UE073", validation.getMessages().get(0).getCode());
        assertEquals("Для адреса с кодом 10 и уровнем 3 не переданы: код района Москвы (areaOMKTE/code); код округа (regionOMKTE/code);", validation.getMessages().get(0).getMessage());
    }

    @Test // aoLevel <> 2 и не передан код района Москвы (areaOMKTE/code)
    public void checkAddressFLKV3FindAoLevelNotTwo() {
        List<AddressRegistry> list = new ArrayList<>();
        AddressRegistry addressRegistry = new AddressRegistry();
        addressRegistry.setGlobalIdNsi(10L);
        addressRegistry.setAoLevel("3"); // aoLevel <> 2

        list.add(addressRegistry);
        Validation validation = new Validation();

        algorithms.checkAddressFLKV3(list, validation);

        assertFalse(validation.isSuccess());
        assertNotNull(validation.getMessages());
        assertEquals(1, validation.getMessages().size());
        assertEquals("UE073", validation.getMessages().get(0).getCode());
        assertEquals("Для адреса с кодом 10 и уровнем 3 не переданы: код района Москвы (areaOMKTE/code); код округа (regionOMKTE/code);", validation.getMessages().get(0).getMessage());
    }

    @Test // aoLevel = 3 и не передан код района (area/code)
    public void checkAddressFLKV3FindAoLevel3() {
        List<AddressRegistry> list = new ArrayList<>();
        AddressRegistry addressRegistry = new AddressRegistry();
        addressRegistry.setGlobalIdNsi(10L);
        addressRegistry.setAoLevel("3");

        list.add(addressRegistry);
        Validation validation = new Validation();

        algorithms.checkAddressFLKV3(list, validation);

        assertFalse(validation.isSuccess());
        assertNotNull(validation.getMessages());
        assertEquals(1, validation.getMessages().size());
        assertEquals("UE073", validation.getMessages().get(0).getCode());
        assertEquals("Для адреса с кодом 10 и уровнем 3 не переданы: код района Москвы (areaOMKTE/code); код округа (regionOMKTE/code);", validation.getMessages().get(0).getMessage());
    }

    @Test // aoLevel = 4 и не передан код города (city/code)
    public void checkAddressFLKV3FindAoLevel4() {
        List<AddressRegistry> list = new ArrayList<>();
        AddressRegistry addressRegistry = new AddressRegistry();
        addressRegistry.setGlobalIdNsi(10L);
        addressRegistry.setAoLevel("4");
        addressRegistry.setAreaOMKTE(new AreaOMKTE() {{
            setCode("001");
        }});

        list.add(addressRegistry);
        Validation validation = new Validation();

        algorithms.checkAddressFLKV3(list, validation);

        assertFalse(validation.isSuccess());
        assertNotNull(validation.getMessages());
        assertEquals(1, validation.getMessages().size());
        assertEquals("UE073", validation.getMessages().get(0).getCode());
        assertEquals("Для адреса с кодом 10 и уровнем 4 не переданы: код города (city/code); код округа (regionOMKTE/code);", validation.getMessages().get(0).getMessage());
    }

    @Test // aoLevel = 6 и не передан код населенного пункта (place/code)
    public void checkAddressFLKV3FindAoLevel6() {
        List<AddressRegistry> list = new ArrayList<>();
        AddressRegistry addressRegistry = new AddressRegistry();
        addressRegistry.setGlobalIdNsi(10L);
        addressRegistry.setAoLevel("6");
        addressRegistry.setAreaOMKTE(new AreaOMKTE() {{
            setCode("001");
        }});

        list.add(addressRegistry);
        Validation validation = new Validation();

        algorithms.checkAddressFLKV3(list, validation);

        assertFalse(validation.isSuccess());
        assertNotNull(validation.getMessages());
        assertEquals(1, validation.getMessages().size());
        assertEquals("UE073", validation.getMessages().get(0).getCode());
        assertEquals("Для адреса с кодом 10 и уровнем 6 не переданы: код населенного пункта (place/code); код округа (regionOMKTE/code);", validation.getMessages().get(0).getMessage());
    }

    @Test // aoLevel = 65 и не передан код планировочной структуры (plan/code)
    public void checkAddressFLKV3FindAoLevel65() {
        List<AddressRegistry> list = new ArrayList<>();
        AddressRegistry addressRegistry = new AddressRegistry();
        addressRegistry.setGlobalIdNsi(10L);
        addressRegistry.setAoLevel("65");
        addressRegistry.setAreaOMKTE(new AreaOMKTE() {{
            setCode("001");
        }});

        list.add(addressRegistry);
        Validation validation = new Validation();

        algorithms.checkAddressFLKV3(list, validation);

        assertFalse(validation.isSuccess());
        assertNotNull(validation.getMessages());
        assertEquals(1, validation.getMessages().size());
        assertEquals("UE073", validation.getMessages().get(0).getCode());
        assertEquals("Для адреса с кодом 10 и уровнем 65 не переданы: код планировочной структуры (plan/code); код округа (regionOMKTE/code);", validation.getMessages().get(0).getMessage());
    }

    @Test // aoLevel = 7 и не передан код улицы (street/code)
    public void checkAddressFLKV3FindAoLevel7() {
        List<AddressRegistry> list = new ArrayList<>();
        AddressRegistry addressRegistry = new AddressRegistry();
        addressRegistry.setGlobalIdNsi(10L);
        addressRegistry.setAoLevel("7");
        addressRegistry.setAreaOMKTE(new AreaOMKTE() {{
            setCode("001");
        }});

        list.add(addressRegistry);
        Validation validation = new Validation();

        algorithms.checkAddressFLKV3(list, validation);

        assertFalse(validation.isSuccess());
        assertNotNull(validation.getMessages());
        assertEquals(1, validation.getMessages().size());
        assertEquals("UE073", validation.getMessages().get(0).getCode());
        assertEquals("Для адреса с кодом 10 и уровнем 7 не переданы: код улицы (street/code); код округа (regionOMKTE/code);", validation.getMessages().get(0).getMessage());
    }


    @Test // aoLevel = 8 не передано значение: дом (building/houseName);
    public void checkAddressFLKV3FindAoLevel8() {
        List<AddressRegistry> list = new ArrayList<>();

        AddressRegistry addressRegistry = new AddressRegistry();
        addressRegistry.setGlobalIdNsi(10L);
        addressRegistry.setAoLevel("8");
        addressRegistry.setAreaOMKTE(new AreaOMKTE() {{
            setCode("001");
        }});

        list.add(addressRegistry);
        Validation validation = new Validation();

        algorithms.checkAddressFLKV3(list, validation);

        assertFalse(validation.isSuccess());
        assertNotNull(validation.getMessages());
        assertEquals(1, validation.getMessages().size());
        assertEquals("UE073", validation.getMessages().get(0).getCode());
        assertEquals("Для адреса с кодом 10 и уровнем 8 не переданы: код дома (building/house/name) или корпуса (building/build/name) или строения (building/construction/name); код округа (regionOMKTE/code);", validation.getMessages().get(0).getMessage());
    }

    @Test
    // aoLevel = 8 и в полях код округа (regionOMKTE/code) и код района Москвы (areaOMKTE/code) указаны не единичные значения
    public void checkAddressFLKV3FindAoLevel88() {
        List<AddressRegistry> addressRegistries = new ArrayList<>();

        AddressRegistry addressRegistry = new AddressRegistry();
        addressRegistry.setGlobalIdNsi(10L);
        addressRegistry.setAoLevel("8");
        addressRegistry.setAreaOMKTE(new AreaOMKTE() {{
            setCode("areaOMKTE;code");
        }});
        addressRegistry.setRegionOMKTE(new RegionOMKTE() {{
            setCode("regionOMKTE;code");
        }});
        addressRegistry.setBuilding(new Building() {{
            setHouse(new House() {{
                setName("house name");
            }});
            setBuild(new Build() {{
                setName("build name");
            }});
            setConstruction(new Construction() {{
                setName("construction name");
            }});
        }});

        addressRegistries.add(addressRegistry);
        Validation validation = new Validation();

        algorithms.checkAddressFLKV3(addressRegistries, validation);

        assertFalse(validation.isSuccess());
        assertNotNull(validation.getMessages());
        assertEquals(1, validation.getMessages().size());
        assertEquals("UE076", validation.getMessages().get(0).getCode());
        assertEquals("Для дома должны быть указаны только один район и округ Москвы", validation.getMessages().get(0).getMessage());
    }

    @Test // система для текущего адреса создаёт в таблице ADDRESSES запись
    public void checkAddressFLKV3SaveAddresses() {
        List<AddressRegistry> addressRegistries = new ArrayList<>();

        AddressRegistry addressRegistry = new AddressRegistry();
        addressRegistry.setGlobalIdNsi(10L);
        addressRegistry.setAoLevel("2");
        addressRegistry.setAddressString("addressString");
        addressRegistry.setRegion(new Region() {{
            setId("1");
            setCode("region code");
            setName("region name");
        }});
        addressRegistry.setRegionOMKTE(new RegionOMKTE() {{
            setId("1");
            setCode("regionOMKTE code");
            setName("regionOMKTE name");
            setType(new Names() {{
                setFull("regionOMKTE full");
                setShort("regionOMKTE short");
            }});
        }});
        addressRegistry.setAreaOMKTE(new AreaOMKTE() {{
            setId("1");
            setCode("areaOMKTE code");
            setName("areaOMKTE name");
            setType(new Names() {{
                setFull("areaOMKTE full");
                setShort("areaOMKTE short");
            }});
        }});
        addressRegistry.setArea(new Area() {{
            setId("1");
            setCode("area code");
            setCodeBTI("area codeBTI");
            setName("area name");
            setType(new Names() {{
                setFull("area full");
                setShort("area short");
            }});
        }});
        addressRegistry.setCity(new City() {{
            setId("1");
            setCode("city code");
            setCodeBTI("city codeBTI");
            setName("city name");
            setType(new Names() {{
                setFull("city full");
                setShort("city short");
            }});
        }});
        addressRegistry.setPlace(new Place() {{
            setId("1");
            setCode("place code");
            setCodeBTI("place codeBTI");
            setName("place name");
            setType(new Names() {{
                setFull("place full");
                setShort("place short");
            }});
        }});
        addressRegistry.setPlan(new Plan() {{
            setId("1");
            setCode("plan code");
            setCodeBTI("plan codeBTI");
            setName("plan name");
            setType(new Names() {{
                setFull("plan full");
                setShort("plan short");
            }});
        }});
        addressRegistry.setStreet(new Street() {{
            setId("1");
            setCode("street code");
            setCodeBTI("street codeBTI");
            setName("street name");
            setCodeOMKUM("street codeOMKUM");
            setType(new Names() {{
                setFull("street full");
                setShort("street short");
            }});
        }});
        addressRegistry.setBuilding(new Building() {{
            setHouse(new House() {{
                setName("house name");
                setType(new Names() {{
                    setFull("house full");
                    setShort("house short");
                }});
            }});
            setBuild(new Build() {{
                setName("build name");
                setType(new Names() {{
                    setFull("build full");
                    setShort("build short");
                }});
            }});
            setConstruction(new Construction() {{
                setName("construction name");
                setType(new Names() {{
                    setFull("construction full");
                    setShort("construction short");
                }});
            }});
        }});

        List<Addresses> addressesList = new ArrayList<>();
        Addresses addresses = new Addresses();
        addresses.setId(1L);
        addresses.setGlobalId(addressRegistry.getGlobalIdNsi());
        addresses.setAoLevel(addressRegistry.getAoLevel());
        addresses.setAddress(addressRegistry.getAddressString());

        addresses.setRegionId(Long.parseLong(addressRegistry.getRegion().getId()));
        addresses.setRegionCode(addressRegistry.getRegion().getCode());
        addresses.setRegionName(addressRegistry.getRegion().getName());
        addresses.setRegionId(Long.parseLong(addressRegistry.getRegionOMKTE().getId()));
        addresses.setRegionTeCode(addressRegistry.getRegionOMKTE().getCode());
        addresses.setRegionTeName(addressRegistry.getRegionOMKTE().getName());
        addresses.setRegionTeTypeName(addressRegistry.getRegionOMKTE().getType().getFull());
        addresses.setRegionTeTypeNameShort(addressRegistry.getRegionOMKTE().getType().getShort());

        addresses.setAreaTeId(addressRegistry.getAreaOMKTE().getId());
        addresses.setAreaCodeOmkTe(addressRegistry.getAreaOMKTE().getCode());
        addresses.setAreaTeName(addressRegistry.getAreaOMKTE().getName());
        addresses.setAreaTeTypeName(addressRegistry.getAreaOMKTE().getType().getFull());
        addresses.setAreaTeTypeNameShort(addressRegistry.getAreaOMKTE().getType().getShort());

        addresses.setAreaId(Long.parseLong(addressRegistry.getArea().getId()));
        addresses.setAreaCode(addressRegistry.getArea().getCode());
        addresses.setAreaBtiCode(addressRegistry.getArea().getCodeBTI());
        addresses.setAreaName(addressRegistry.getArea().getName());
        addresses.setAreaTypeName(addressRegistry.getArea().getType().getFull());
        addresses.setAreaTypeNameShort(addressRegistry.getArea().getType().getShort());

        addresses.setCityId(Long.parseLong(addressRegistry.getCity().getId()));
        addresses.setCityCode(addressRegistry.getCity().getCode());
        addresses.setCityBtiCode(addressRegistry.getCity().getCodeBTI());
        addresses.setCityName(addressRegistry.getCity().getName());
        addresses.setCityTypeName(addressRegistry.getCity().getType().getFull());
        addresses.setCityTypeNameShort(addressRegistry.getCity().getType().getShort());

        addresses.setPlaceId(Long.parseLong(addressRegistry.getPlace().getId()));
        addresses.setPlaceCode(addressRegistry.getPlace().getCode());
        addresses.setPlaceBtiCode(addressRegistry.getPlace().getCodeBTI());
        addresses.setPlaceName(addressRegistry.getPlace().getName());
        addresses.setPlaceTypeName(addressRegistry.getPlace().getType().getFull());
        addresses.setPlaceTypeNameShort(addressRegistry.getPlace().getType().getShort());

        addresses.setPlanId(Long.parseLong(addressRegistry.getPlan().getId()));
        addresses.setPlanCode(addressRegistry.getPlan().getCode());
        addresses.setPlanBtiCode(addressRegistry.getPlan().getCodeBTI());
        addresses.setPlanName(addressRegistry.getPlan().getName());
        addresses.setPlanTypeName(addressRegistry.getPlan().getType().getFull());
        addresses.setPlanTypeNameShort(addressRegistry.getPlan().getType().getShort());

        addresses.setStreetId(Long.parseLong(addressRegistry.getStreet().getId()));
        addresses.setStreetCode(addressRegistry.getStreet().getCode());
        addresses.setStreetBtiCode(addressRegistry.getStreet().getCodeBTI());
        addresses.setStreetOmkUm(addressRegistry.getStreet().getCodeOMKUM());
        addresses.setStreetName(addressRegistry.getStreet().getName());
        addresses.setStreetTypeName(addressRegistry.getStreet().getType().getFull());
        addresses.setStreetTypeNameShort(addressRegistry.getStreet().getType().getShort());

        addresses.setL1Type(addressRegistry.getBuilding().getHouse().getType().getFull());
        addresses.setL1TypeShort(addressRegistry.getBuilding().getHouse().getType().getShort());
        addresses.setL1Value(addressRegistry.getBuilding().getHouse().getName());

        addresses.setL2Type(addressRegistry.getBuilding().getBuild().getType().getFull());
        addresses.setL2TypeShort(addressRegistry.getBuilding().getBuild().getType().getShort());
        addresses.setL2Value(addressRegistry.getBuilding().getBuild().getName());

        addresses.setL3Type(addressRegistry.getBuilding().getConstruction().getType().getFull());
        addresses.setL3TypeShort(addressRegistry.getBuilding().getConstruction().getType().getShort());
        addresses.setL3Value(addressRegistry.getBuilding().getConstruction().getName());

        addresses.setUpdateDate(LocalDateTime.now());

        addressRegistries.add(addressRegistry);
        addressesList.add(addresses);

        Validation validation = new Validation();
        List<Addresses> addressFLKV3 = algorithms.checkAddressFLKV3(addressRegistries, validation);

        assertNotNull(addressFLKV3);
        assertEquals(1, addressFLKV3.size());
        assertEquals(10L, addressFLKV3.get(0).getGlobalId().longValue());
    }

    @Test
    public void checkAddNewAddress() {
        List<AddressRegistry> addressRegistries = new ArrayList<>();
        AddressRegistry newAddress = new AddressRegistry();
        newAddress.setAoLevel("3");
        newAddress.setGlobalIdNsi(123L);
        newAddress.setArea(new Area() {{
            setCode("111");
        }});
        newAddress.setRegionOMKTE(new RegionOMKTE() {{
            setCode("123456");
        }});
        newAddress.setAreaOMKTE(new AreaOMKTE() {{
            setCode("654321");
        }});
        newAddress.setAddressString("test");

        addressRegistries.add(newAddress);

        Validation validation = new Validation();
        List<Addresses> addressFLKV3 = algorithms.checkAddressFLKV3(addressRegistries, validation);

        assertTrue(validation.isSuccess());
        assertEquals(1, addressFLKV3.size());
        assertEquals(123, addressFLKV3.get(0).getGlobalId().longValue());
        assertEquals("111", addressFLKV3.get(0).getAreaCode());
        assertEquals("3", addressFLKV3.get(0).getAoLevel());

        List<Addresses> findAddresses = addressesRepository.findAddresses(addressFLKV3.stream().map(item -> item.getGlobalId()).collect(Collectors.toList()));
        assertEquals(1, findAddresses.size());
        assertEquals(addressFLKV3.get(0).getId(), findAddresses.get(0).getId());
        assertEquals(addressFLKV3.get(0).getGlobalId().longValue(), findAddresses.get(0).getGlobalId().longValue());
        assertEquals(addressFLKV3.get(0).getAreaCode(), findAddresses.get(0).getAreaCode());
        assertEquals(addressFLKV3.get(0).getAoLevel(), findAddresses.get(0).getAoLevel());
        assertEquals(addressFLKV3.get(0).getRegionCode(), findAddresses.get(0).getRegionCode());
        assertEquals(addressFLKV3.get(0).getAreaCode(), findAddresses.get(0).getAreaCode());
    }

    @Test // добавляем один существующий адрес и один не существующий: 1 добавится 1 вернется, вернется 2 элемента
    @Sql(scripts = {"/sql/checkAddressFLKV3FindAddresses.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addOneAddAddressExistOneNotExist() {
        List<AddressRegistry> addressRegistries = new ArrayList<>();

        AddressRegistry newAddress1 = new AddressRegistry();
        newAddress1.setGlobalIdNsi(123L);
        newAddress1.setAoLevel("3");
        newAddress1.setArea(new Area() {{
            setCode("111");
        }});
        newAddress1.setRegionOMKTE(new RegionOMKTE() {{
            setCode("123456");
        }});
        newAddress1.setAreaOMKTE(new AreaOMKTE() {{
            setCode("333");
        }});
        newAddress1.setAddressString("address");

        AddressRegistry newAddress2 = new AddressRegistry();
        newAddress2.setGlobalIdNsi(10L);
        newAddress2.setAoLevel("3");
        newAddress2.setArea(new Area() {{
            setCode("444");
        }});
        newAddress2.setAreaOMKTE(new AreaOMKTE() {{
            setCode("666");
        }});
        newAddress2.setRegionOMKTE(new RegionOMKTE() {{
            setCode("555");
        }});
        newAddress2.setAddressString("address1");

        addressRegistries.add(newAddress1);
        addressRegistries.add(newAddress2);

        Validation validation = new Validation();
        List<Addresses> addressFLKV3 = algorithms.checkAddressFLKV3(addressRegistries, validation);

        assertTrue(validation.isSuccess());
        assertEquals(2, addressFLKV3.size());

        List<Addresses> findAddresses = addressesRepository.findAddresses(addressFLKV3.stream().map(Addresses::getGlobalId).collect(Collectors.toList()));
        assertEquals(2, findAddresses.size());
        findAddresses.sort(Comparator.comparingLong(Addresses::getGlobalId));
        addressFLKV3.sort(Comparator.comparingLong(Addresses::getGlobalId));
        for (int i = 0; i < findAddresses.size(); i++) {
            assertEquals(addressFLKV3.get(i).getId(), findAddresses.get(i).getId());
            assertEquals(addressFLKV3.get(i).getGlobalId().longValue(), findAddresses.get(i).getGlobalId().longValue());
            assertEquals(addressFLKV3.get(i).getAreaCode(), findAddresses.get(i).getAreaCode());
            assertEquals(addressFLKV3.get(i).getAoLevel(), findAddresses.get(i).getAoLevel());
            assertEquals(addressFLKV3.get(i).getRegionCode(), findAddresses.get(i).getRegionCode());
            assertEquals(addressFLKV3.get(i).getAreaCode(), findAddresses.get(i).getAreaCode());
        }
    }

    @Test // добавляем элемент который уже есть: возвращается существующий элемент
    @Sql(scripts = {"/sql/checkAddressFLKV3FindAddresses.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addAddressExist() {
        List<AddressRegistry> addressRegistries = new ArrayList<>();

        AddressRegistry newAddress1 = new AddressRegistry();
        newAddress1.setGlobalIdNsi(123L);
        newAddress1.setAoLevel("3");
        newAddress1.setArea(new Area() {{
            setCode("111");
        }});
        newAddress1.setRegionOMKTE(new RegionOMKTE() {{
            setCode("123456");
        }});
        newAddress1.setAreaOMKTE(new AreaOMKTE() {{
            setCode("333");
        }});
        newAddress1.setAddressString("address");

        addressRegistries.add(newAddress1);

        Validation validation = new Validation();
        List<Addresses> addressFLKV3 = algorithms.checkAddressFLKV3(addressRegistries, validation);

        assertTrue(validation.isSuccess());
        assertEquals(1, addressFLKV3.size());

        List<Addresses> findAddresses = addressesRepository.findAddresses(addressFLKV3.stream().map(Addresses::getGlobalId).collect(Collectors.toList()));
        assertEquals(1, findAddresses.size());
        findAddresses.sort(Comparator.comparingLong(Addresses::getGlobalId));
        addressFLKV3.sort(Comparator.comparingLong(Addresses::getGlobalId));
        for (int i = 0; i < findAddresses.size(); i++) {
            assertEquals(addressFLKV3.get(i).getId(), findAddresses.get(i).getId());
            assertEquals(addressFLKV3.get(i).getGlobalId().longValue(), findAddresses.get(i).getGlobalId().longValue());
            assertEquals(addressFLKV3.get(i).getAreaCode(), findAddresses.get(i).getAreaCode());
            assertEquals(addressFLKV3.get(i).getAoLevel(), findAddresses.get(i).getAoLevel());
            assertEquals(addressFLKV3.get(i).getRegionCode(), findAddresses.get(i).getRegionCode());
            assertEquals(addressFLKV3.get(i).getAreaCode(), findAddresses.get(i).getAreaCode());
        }
    }

    @Test
    public void addAddressAoLvl1() {
        List<AddressRegistry> addressRegistries = new ArrayList<>();

        AddressRegistry newAddress1 = new AddressRegistry();
        newAddress1.setGlobalIdNsi(123L);
        newAddress1.setAoLevel("1");
        newAddress1.setArea(new Area() {{
            setCode("111");
        }});
        newAddress1.setAreaOMKTE(new AreaOMKTE() {{
            setCode("333");
        }});
        newAddress1.setAddressString("address");

        addressRegistries.add(newAddress1);

        Validation validation = new Validation();
        algorithms.checkAddressFLKV3(addressRegistries, validation);

        assertFalse(validation.isSuccess());
        assertEquals("Некорректный уровень адреса (1)", validation.getMessages().get(0).getMessage());
    }

    @Test
    public void addAddressAoLvl2() {
        List<AddressRegistry> addressRegistries = new ArrayList<>();

        AddressRegistry newAddress1 = new AddressRegistry();
        newAddress1.setGlobalIdNsi(123L);
        newAddress1.setAoLevel("2");
        newAddress1.setArea(new Area() {{
            setCode("111");
        }});
        newAddress1.setRegionOMKTE(new RegionOMKTE() {{
            setCode("333");
        }});
        newAddress1.setAddressString("address");

        addressRegistries.add(newAddress1);

        Validation validation = new Validation();
        algorithms.checkAddressFLKV3(addressRegistries, validation);

        assertTrue(validation.isSuccess());
    }
}
