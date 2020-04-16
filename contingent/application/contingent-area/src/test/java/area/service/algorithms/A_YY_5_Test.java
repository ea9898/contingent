package area.service.algorithms;

import moscow.ptnl.contingent.domain.area.entity.Addresses;
import moscow.ptnl.contingent.domain.area.entity.Area;
import moscow.ptnl.contingent.domain.area.entity.AreaAddress;
import moscow.ptnl.contingent.nsi.domain.area.AreaType;
import moscow.ptnl.contingent2.area.info.AreaInfoEvent;
import org.junit.jupiter.api.Test;
import service.BaseTest;

import static org.jgroups.util.Util.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

public class A_YY_5_Test extends BaseTest {

    @Test
    public void area_mapping_mu() {
        AreaType areaType = new AreaType();
        areaType.setCode(10L);
        areaType.setResidentsBindRate(100);
        Area area = new Area();
        area.setId(100L);
        area.setMoId(1L);
        area.setMuId(2L);
        area.setDescription("Test area");
        area.setAreaType(areaType);
        area.setArchived(false);
        area.setNumber(101);
        area.setAutoAssignForAttach(true);
        AreaInfoEvent areaInfoEvent = areaInfoEventMapper.entityToDtoTransform(
                algorithms.createTopicAreaInfo(area, "createAttachment"));

        assertEquals(areaInfoEvent.getOperationType(), "createAttachment");
        assertEquals(areaInfoEvent.getAreaId(), area.getId());
        assertEquals(areaInfoEvent.getAreaType(), areaType.getCode());
        assertEquals(areaInfoEvent.getMuId(), area.getMuId());
        assertEquals(areaInfoEvent.getNumber(), Long.valueOf(area.getNumber()));
        assertEquals(areaInfoEvent.getName(), "Test area");
        assertEquals(areaInfoEvent.isArchive(), !area.isActual());
        assertEquals(areaInfoEvent.isAutoAssignForAttachment(), area.getAutoAssignForAttach());
        assertEquals(areaInfoEvent.getResidentsBindRate(), Long.valueOf(areaType.getResidentsBindRate()));
    }

    @Test
    public void area_mapping_mo() {
        AreaType areaType = new AreaType();
        areaType.setCode(10L);
        areaType.setResidentsBindRate(100);
        Area area = new Area();
        area.setId(100L);
        area.setMoId(1L);
        area.setDescription("Test area");
        area.setAreaType(areaType);
        area.setArchived(false);
        area.setNumber(101);
        area.setAutoAssignForAttach(true);
        AreaInfoEvent areaInfoEvent = areaInfoEventMapper.entityToDtoTransform(algorithms.createTopicAreaInfo(area, "createAttachment"));

        assertEquals(areaInfoEvent.getOperationType(), "createAttachment");
        assertEquals(areaInfoEvent.getAreaId(), area.getId());
        assertEquals(areaInfoEvent.getAreaType(), areaType.getCode());
        assertEquals(areaInfoEvent.getMuId(), area.getMoId());
        assertEquals(areaInfoEvent.getNumber(), Long.valueOf(area.getNumber()));
        assertEquals(areaInfoEvent.getName(), "Test area");
        assertEquals(areaInfoEvent.isArchive(), !area.isActual());
        assertEquals(areaInfoEvent.isAutoAssignForAttachment(), area.getAutoAssignForAttach());
        assertEquals(areaInfoEvent.getResidentsBindRate(), Long.valueOf(areaType.getResidentsBindRate()));
    }

    // AreaRestriction
    @Test
    public void area_restriction() {
        AreaType areaType = new AreaType();
        areaType.setCode(10L);
        areaType.setResidentsBindRate(100);
        areaType.setGender("MALE");
        areaType.setAgeMin(10);
        areaType.setAgeMax(100);
        areaType.setAgeMMin(11);
        areaType.setAgeMMax(101);
        areaType.setAgeWMin(12);
        areaType.setAgeWMax(102);

        Area area = new Area();
        area.setId(100L);
        area.setMoId(1L);
        area.setDescription("Test area");
        area.setAreaType(areaType);
        area.setArchived(false);
        area.setNumber(101);
        area.setAutoAssignForAttach(true);

        AreaInfoEvent areaInfoEvent = areaInfoEventMapper.entityToDtoTransform(algorithms.createTopicAreaInfo(area, "createAttachment"));

        assertEquals(areaInfoEvent.getAreaRestriction().getGender(), areaType.getGender());
        assertEquals(areaInfoEvent.getAreaRestriction().getMinAge(), areaType.getAgeMin());
        assertEquals(areaInfoEvent.getAreaRestriction().getMaxAge(), areaType.getAgeMax());
        assertEquals(areaInfoEvent.getAreaRestriction().getMinAgeMale(), areaType.getAgeMMin());
        assertEquals(areaInfoEvent.getAreaRestriction().getMaxAgeMale(), areaType.getAgeMMax());
        assertEquals(areaInfoEvent.getAreaRestriction().getMinAgeFemale(), areaType.getAgeWMin());
        assertEquals(areaInfoEvent.getAreaRestriction().getMaxAgeFemale(), areaType.getAgeWMax());

    }

    // AreaRestriction
    @Test
    public void area_restriction_2() {
        AreaType areaType = new AreaType();
        areaType.setCode(10L);
        areaType.setResidentsBindRate(100);
        areaType.setGender("MALE");
        areaType.setAgeMin(10);
        areaType.setAgeMax(100);

        Area area = new Area();
        area.setId(100L);
        area.setMoId(1L);
        area.setDescription("Test area");
        area.setAreaType(areaType);
        area.setArchived(false);
        area.setNumber(101);
        area.setAutoAssignForAttach(true);
        area.setAgeMin(20);
        area.setAgeMax(120);
        area.setAgeMMin(11);
        area.setAgeMMax(101);
        area.setAgeWMin(12);
        area.setAgeWMax(102);

        AreaInfoEvent areaInfoEvent = areaInfoEventMapper.entityToDtoTransform(algorithms.createTopicAreaInfo(area, "createAttachment"));

        assertEquals(areaInfoEvent.getAreaRestriction().getGender(), areaType.getGender());
        assertEquals(areaInfoEvent.getAreaRestriction().getMinAge(), area.getAgeMin());
        assertEquals(areaInfoEvent.getAreaRestriction().getMaxAge(), area.getAgeMax());
        assertEquals(areaInfoEvent.getAreaRestriction().getMinAgeMale(), area.getAgeMMin());
        assertEquals(areaInfoEvent.getAreaRestriction().getMaxAgeMale(), area.getAgeMMax());
        assertEquals(areaInfoEvent.getAreaRestriction().getMinAgeFemale(), area.getAgeWMin());
        assertEquals(areaInfoEvent.getAreaRestriction().getMaxAgeFemale(), area.getAgeWMax());
    }


    // MainEmployees

    // ReplacementEmployees

    // Addresses
    @Test
    public void area_addresses_level_8() {

        AreaType areaType = new AreaType();
        areaType.setCode(10L);
        areaType.setResidentsBindRate(100);
        areaType.setGender("MALE");

        Area area = new Area();
        area.setId(100L);
        area.setMoId(1L);
        area.setDescription("Test area");
        area.setAreaType(areaType);
        area.setArchived(false);
        area.setNumber(101);
        area.setAutoAssignForAttach(true);

        Addresses addresses = new Addresses();
        addresses.setAoLevel("8");
        addresses.setRegionTeCode("teCode");
        addresses.setAreaCodeOmkTe("areaTeCode");
        addresses.setStreetOmkUm("10000");
        addresses.setStreetBtiCode("bti1;bti2");
        addresses.setPlanBtiCode("bti1;bti2;bti3");
        addresses.setPlaceBtiCode("bti1;bti2;bti3;bti4");
        addresses.setCityBtiCode("bti1;bti2;bti3;bti4;bti5");
        addresses.setAreaBtiCode("bti1;bti2;bti3;bti4;bti5;bti6");
        addresses.setL1Value("House");
        addresses.setL2Value("Building");
        addresses.setL3Value("Construction");

        AreaAddress areaAddress = new AreaAddress();
        areaAddress.setAddress(addresses);
        areaAddress.setArea(area);

        area.getAreaAddresses().add(areaAddress);

        AreaInfoEvent areaInfoEvent = areaInfoEventMapper.entityToDtoTransform(algorithms.createTopicAreaInfo(area, "createAttachment"));

        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getAolevel(), addresses.getAoLevel());
        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getOmkUmCode(), addresses.getStreetOmkUm());
        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getOmkTeDistrictCode(), addresses.getRegionTeCode());
        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getOmkTeRegionCode(), addresses.getAreaCodeOmkTe());

        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getStreetBTI().getCode().size(), 2);

        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getHouse(), addresses.getL1Value());
        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getBuilding(), addresses.getL2Value());
        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getConstruction(), addresses.getL3Value());

    }

    @Test
    public void area_addresses_level_8_1() {

        AreaType areaType = new AreaType();
        areaType.setCode(10L);
        areaType.setResidentsBindRate(100);
        areaType.setGender("MALE");

        Area area = new Area();
        area.setId(100L);
        area.setMoId(1L);
        area.setDescription("Test area");
        area.setAreaType(areaType);
        area.setArchived(false);
        area.setNumber(101);
        area.setAutoAssignForAttach(true);

        Addresses addresses = new Addresses();
        addresses.setAoLevel("8");
        addresses.setRegionTeCode("teCode");
        addresses.setAreaCodeOmkTe("areaTeCode");
        addresses.setStreetOmkUm("10000");
        addresses.setPlanBtiCode("bti1;bti2;bti3");
        addresses.setPlaceBtiCode("bti1;bti2;bti3;bti4");
        addresses.setCityBtiCode("bti1;bti2;bti3;bti4;bti5");
        addresses.setAreaBtiCode("bti1;bti2;bti3;bti4;bti5;bti6");
        addresses.setL1Value("House");
        addresses.setL2Value("Building");
        addresses.setL3Value("Construction");

        AreaAddress areaAddress = new AreaAddress();
        areaAddress.setAddress(addresses);
        areaAddress.setArea(area);

        area.getAreaAddresses().add(areaAddress);

        AreaInfoEvent areaInfoEvent = areaInfoEventMapper.entityToDtoTransform(algorithms.createTopicAreaInfo(area, "createAttachment"));

        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getAolevel(), addresses.getAoLevel());
        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getOmkUmCode(), addresses.getStreetOmkUm());
        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getOmkTeDistrictCode(), addresses.getRegionTeCode());
        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getOmkTeRegionCode(), addresses.getAreaCodeOmkTe());

        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getStreetBTI().getCode().size(), 3);

        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getHouse(), addresses.getL1Value());
        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getBuilding(), addresses.getL2Value());
        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getConstruction(), addresses.getL3Value());

    }
    @Test
    public void area_addresses_level_8_2() {

        AreaType areaType = new AreaType();
        areaType.setCode(10L);
        areaType.setResidentsBindRate(100);
        areaType.setGender("MALE");

        Area area = new Area();
        area.setId(100L);
        area.setMoId(1L);
        area.setDescription("Test area");
        area.setAreaType(areaType);
        area.setArchived(false);
        area.setNumber(101);
        area.setAutoAssignForAttach(true);

        Addresses addresses = new Addresses();
        addresses.setAoLevel("8");
        addresses.setRegionTeCode("teCode");
        addresses.setAreaCodeOmkTe("areaTeCode");
        addresses.setStreetOmkUm("10000");
        addresses.setPlaceBtiCode("bti1;bti2;bti3;bti4");
        addresses.setCityBtiCode("bti1;bti2;bti3;bti4;bti5");
        addresses.setAreaBtiCode("bti1;bti2;bti3;bti4;bti5;bti6");
        addresses.setL1Value("House");
        addresses.setL2Value("Building");
        addresses.setL3Value("Construction");

        AreaAddress areaAddress = new AreaAddress();
        areaAddress.setAddress(addresses);
        areaAddress.setArea(area);

        area.getAreaAddresses().add(areaAddress);

        AreaInfoEvent areaInfoEvent = areaInfoEventMapper.entityToDtoTransform(algorithms.createTopicAreaInfo(area, "createAttachment"));

        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getAolevel(), addresses.getAoLevel());
        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getOmkUmCode(), addresses.getStreetOmkUm());
        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getOmkTeDistrictCode(), addresses.getRegionTeCode());
        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getOmkTeRegionCode(), addresses.getAreaCodeOmkTe());

        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getStreetBTI().getCode().size(), 4);

        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getHouse(), addresses.getL1Value());
        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getBuilding(), addresses.getL2Value());
        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getConstruction(), addresses.getL3Value());

    }
    @Test
    public void area_addresses_level_8_3() {

        AreaType areaType = new AreaType();
        areaType.setCode(10L);
        areaType.setResidentsBindRate(100);
        areaType.setGender("MALE");

        Area area = new Area();
        area.setId(100L);
        area.setMoId(1L);
        area.setDescription("Test area");
        area.setAreaType(areaType);
        area.setArchived(false);
        area.setNumber(101);
        area.setAutoAssignForAttach(true);

        Addresses addresses = new Addresses();
        addresses.setAoLevel("8");
        addresses.setRegionTeCode("teCode");
        addresses.setAreaCodeOmkTe("areaTeCode");
        addresses.setStreetOmkUm("10000");
        addresses.setCityBtiCode("bti1;bti2;bti3;bti4;bti5");
        addresses.setAreaBtiCode("bti1;bti2;bti3;bti4;bti5;bti6");
        addresses.setL1Value("House");
        addresses.setL2Value("Building");
        addresses.setL3Value("Construction");

        AreaAddress areaAddress = new AreaAddress();
        areaAddress.setAddress(addresses);
        areaAddress.setArea(area);

        area.getAreaAddresses().add(areaAddress);

        AreaInfoEvent areaInfoEvent = areaInfoEventMapper.entityToDtoTransform(algorithms.createTopicAreaInfo(area, "createAttachment"));

        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getAolevel(), addresses.getAoLevel());
        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getOmkUmCode(), addresses.getStreetOmkUm());
        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getOmkTeDistrictCode(), addresses.getRegionTeCode());
        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getOmkTeRegionCode(), addresses.getAreaCodeOmkTe());

        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getStreetBTI().getCode().size(), 5);

        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getHouse(), addresses.getL1Value());
        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getBuilding(), addresses.getL2Value());
        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getConstruction(), addresses.getL3Value());

    }
    @Test
    public void area_addresses_level_8_4() {

        AreaType areaType = new AreaType();
        areaType.setCode(10L);
        areaType.setResidentsBindRate(100);
        areaType.setGender("MALE");

        Area area = new Area();
        area.setId(100L);
        area.setMoId(1L);
        area.setDescription("Test area");
        area.setAreaType(areaType);
        area.setArchived(false);
        area.setNumber(101);
        area.setAutoAssignForAttach(true);

        Addresses addresses = new Addresses();
        addresses.setAoLevel("8");
        addresses.setRegionTeCode("teCode");
        addresses.setAreaCodeOmkTe("areaTeCode");
        addresses.setStreetOmkUm("10000");
        addresses.setAreaBtiCode("bti1;bti2;bti3;bti4;bti5;bti6");
        addresses.setL1Value("House");
        addresses.setL2Value("Building");
        addresses.setL3Value("Construction");

        AreaAddress areaAddress = new AreaAddress();
        areaAddress.setAddress(addresses);
        areaAddress.setArea(area);

        area.getAreaAddresses().add(areaAddress);

        AreaInfoEvent areaInfoEvent = areaInfoEventMapper.entityToDtoTransform(algorithms.createTopicAreaInfo(area, "createAttachment"));

        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getAolevel(), addresses.getAoLevel());
        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getOmkUmCode(), addresses.getStreetOmkUm());
        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getOmkTeDistrictCode(), addresses.getRegionTeCode());
        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getOmkTeRegionCode(), addresses.getAreaCodeOmkTe());

        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getStreetBTI().getCode().size(), 6);

        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getHouse(), addresses.getL1Value());
        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getBuilding(), addresses.getL2Value());
        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getConstruction(), addresses.getL3Value());

    }

    @Test
    public void area_addresses_level_7() {

        AreaType areaType = new AreaType();
        areaType.setCode(10L);
        areaType.setResidentsBindRate(100);
        areaType.setGender("MALE");

        Area area = new Area();
        area.setId(100L);
        area.setMoId(1L);
        area.setDescription("Test area");
        area.setAreaType(areaType);
        area.setArchived(false);
        area.setNumber(101);
        area.setAutoAssignForAttach(true);

        Addresses addresses = new Addresses();
        addresses.setAoLevel("7");
        addresses.setRegionTeCode("teCode");
        addresses.setAreaCodeOmkTe("areaTeCode");
        addresses.setStreetOmkUm("10000");
        addresses.setStreetBtiCode("bti1;bti2");
        addresses.setPlanBtiCode("bti1;bti2;bti3");
        addresses.setPlaceBtiCode("bti1;bti2;bti3;bti4");
        addresses.setCityBtiCode("bti1;bti2;bti3;bti4;bti5");
        addresses.setAreaBtiCode("bti1;bti2;bti3;bti4;bti5;bti6");
        addresses.setL1Value("House");
        addresses.setL2Value("Building");
        addresses.setL3Value("Construction");

        AreaAddress areaAddress = new AreaAddress();
        areaAddress.setAddress(addresses);
        areaAddress.setArea(area);

        area.getAreaAddresses().add(areaAddress);

        AreaInfoEvent areaInfoEvent = areaInfoEventMapper.entityToDtoTransform(algorithms.createTopicAreaInfo(area, "createAttachment"));

        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getAolevel(), addresses.getAoLevel());
        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getOmkUmCode(), addresses.getStreetOmkUm());
        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getOmkTeDistrictCode(), addresses.getRegionTeCode());
        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getOmkTeRegionCode(), addresses.getAreaCodeOmkTe());

        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getStreetBTI().getCode().size(), 2);

        assertNull(areaInfoEvent.getAddresses().getAddress().get(0).getHouse());
        assertNull(areaInfoEvent.getAddresses().getAddress().get(0).getBuilding());
        assertNull(areaInfoEvent.getAddresses().getAddress().get(0).getConstruction());

    }

    @Test
    public void area_addresses_level_7_1() {

        AreaType areaType = new AreaType();
        areaType.setCode(10L);
        areaType.setResidentsBindRate(100);
        areaType.setGender("MALE");

        Area area = new Area();
        area.setId(100L);
        area.setMoId(1L);
        area.setDescription("Test area");
        area.setAreaType(areaType);
        area.setArchived(false);
        area.setNumber(101);
        area.setAutoAssignForAttach(true);

        Addresses addresses = new Addresses();
        addresses.setAoLevel("7");
        addresses.setRegionTeCode("teCode");
        addresses.setAreaCodeOmkTe("areaTeCode");
        addresses.setStreetOmkUm("10000");
        addresses.setStreetBtiCode("bti1;bti2");

        AreaAddress areaAddress = new AreaAddress();
        areaAddress.setAddress(addresses);
        areaAddress.setArea(area);

        area.getAreaAddresses().add(areaAddress);

        AreaInfoEvent areaInfoEvent = areaInfoEventMapper.entityToDtoTransform(algorithms.createTopicAreaInfo(area, "createAttachment"));

        assertEquals(areaInfoEvent.getAddresses().getAddress().size(), 1);
    }

    @Test
    public void area_addresses_level_7_2() {

        AreaType areaType = new AreaType();
        areaType.setCode(10L);
        areaType.setResidentsBindRate(100);
        areaType.setGender("MALE");

        Area area = new Area();
        area.setId(100L);
        area.setMoId(1L);
        area.setDescription("Test area");
        area.setAreaType(areaType);
        area.setArchived(false);
        area.setNumber(101);
        area.setAutoAssignForAttach(true);

        Addresses addresses = new Addresses();
        addresses.setAoLevel("7");
        addresses.setRegionTeCode("teCode");
        addresses.setAreaCodeOmkTe("areaTeCode");
        addresses.setStreetOmkUm("10000");

        AreaAddress areaAddress = new AreaAddress();
        areaAddress.setAddress(addresses);
        areaAddress.setArea(area);

        area.getAreaAddresses().add(areaAddress);

        AreaInfoEvent areaInfoEvent = areaInfoEventMapper.entityToDtoTransform(algorithms.createTopicAreaInfo(area, "createAttachment"));

        assertEquals(areaInfoEvent.getAddresses().getAddress().size(), 1);
    }

    @Test
    public void area_addresses_level_7_3() {

        AreaType areaType = new AreaType();
        areaType.setCode(10L);
        areaType.setResidentsBindRate(100);
        areaType.setGender("MALE");

        Area area = new Area();
        area.setId(100L);
        area.setMoId(1L);
        area.setDescription("Test area");
        area.setAreaType(areaType);
        area.setArchived(false);
        area.setNumber(101);
        area.setAutoAssignForAttach(true);

        Addresses addresses = new Addresses();
        addresses.setAoLevel("7");
        addresses.setRegionTeCode("teCode");
        addresses.setAreaCodeOmkTe("areaTeCode");
        addresses.setStreetBtiCode("bti1;bti2");

        AreaAddress areaAddress = new AreaAddress();
        areaAddress.setAddress(addresses);
        areaAddress.setArea(area);

        area.getAreaAddresses().add(areaAddress);

        AreaInfoEvent areaInfoEvent = areaInfoEventMapper.entityToDtoTransform(algorithms.createTopicAreaInfo(area, "createAttachment"));

        assertEquals(areaInfoEvent.getAddresses().getAddress().size(), 1);
    }

    @Test
    public void area_addresses_level_7_4() {

        AreaType areaType = new AreaType();
        areaType.setCode(10L);
        areaType.setResidentsBindRate(100);
        areaType.setGender("MALE");

        Area area = new Area();
        area.setId(100L);
        area.setMoId(1L);
        area.setDescription("Test area");
        area.setAreaType(areaType);
        area.setArchived(false);
        area.setNumber(101);
        area.setAutoAssignForAttach(true);

        Addresses addresses = new Addresses();
        addresses.setAoLevel("7");
        addresses.setRegionTeCode("teCode");
        addresses.setAreaCodeOmkTe("areaTeCode");

        AreaAddress areaAddress = new AreaAddress();
        areaAddress.setAddress(addresses);
        areaAddress.setArea(area);

        area.getAreaAddresses().add(areaAddress);

        AreaInfoEvent areaInfoEvent = areaInfoEventMapper.entityToDtoTransform(algorithms.createTopicAreaInfo(area, "createAttachment"));

        assertEquals(areaInfoEvent.getAddresses().getAddress().size(), 0);
    }

    @Test
    public void area_addresses_level_65() {

        AreaType areaType = new AreaType();
        areaType.setCode(10L);
        areaType.setResidentsBindRate(100);
        areaType.setGender("MALE");

        Area area = new Area();
        area.setId(100L);
        area.setMoId(1L);
        area.setDescription("Test area");
        area.setAreaType(areaType);
        area.setArchived(false);
        area.setNumber(101);
        area.setAutoAssignForAttach(true);

        Addresses addresses = new Addresses();
        addresses.setAoLevel("65");
        addresses.setRegionTeCode("teCode");
        addresses.setAreaCodeOmkTe("areaTeCode");
        addresses.setStreetOmkUm("10000");
        addresses.setStreetBtiCode("bti1;bti2");
        addresses.setPlanBtiCode("bti1;bti2;bti3");
        addresses.setPlaceBtiCode("bti1;bti2;bti3;bti4");
        addresses.setCityBtiCode("bti1;bti2;bti3;bti4;bti5");
        addresses.setAreaBtiCode("bti1;bti2;bti3;bti4;bti5;bti6");
        addresses.setL1Value("House");
        addresses.setL2Value("Building");
        addresses.setL3Value("Construction");

        AreaAddress areaAddress = new AreaAddress();
        areaAddress.setAddress(addresses);
        areaAddress.setArea(area);

        area.getAreaAddresses().add(areaAddress);

        AreaInfoEvent areaInfoEvent = areaInfoEventMapper.entityToDtoTransform(algorithms.createTopicAreaInfo(area, "createAttachment"));

        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getAolevel(), addresses.getAoLevel());
        assertNull(areaInfoEvent.getAddresses().getAddress().get(0).getOmkUmCode());
        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getOmkTeDistrictCode(), addresses.getRegionTeCode());
        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getOmkTeRegionCode(), addresses.getAreaCodeOmkTe());

        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getStreetBTI().getCode().size(), 3);

        assertNull(areaInfoEvent.getAddresses().getAddress().get(0).getHouse());
        assertNull(areaInfoEvent.getAddresses().getAddress().get(0).getBuilding());
        assertNull(areaInfoEvent.getAddresses().getAddress().get(0).getConstruction());

    }

    @Test
    public void area_addresses_level_65_1() {

        AreaType areaType = new AreaType();
        areaType.setCode(10L);
        areaType.setResidentsBindRate(100);
        areaType.setGender("MALE");

        Area area = new Area();
        area.setId(100L);
        area.setMoId(1L);
        area.setDescription("Test area");
        area.setAreaType(areaType);
        area.setArchived(false);
        area.setNumber(101);
        area.setAutoAssignForAttach(true);

        Addresses addresses = new Addresses();
        addresses.setAoLevel("65");
        addresses.setRegionTeCode("teCode");
        addresses.setAreaCodeOmkTe("areaTeCode");
        addresses.setStreetOmkUm("10000");
        addresses.setStreetBtiCode("bti1;bti2");
        addresses.setPlaceBtiCode("bti1;bti2;bti3;bti4");
        addresses.setCityBtiCode("bti1;bti2;bti3;bti4;bti5");
        addresses.setAreaBtiCode("bti1;bti2;bti3;bti4;bti5;bti6");
        addresses.setL1Value("House");
        addresses.setL2Value("Building");
        addresses.setL3Value("Construction");

        AreaAddress areaAddress = new AreaAddress();
        areaAddress.setAddress(addresses);
        areaAddress.setArea(area);

        area.getAreaAddresses().add(areaAddress);

        AreaInfoEvent areaInfoEvent = areaInfoEventMapper.entityToDtoTransform(algorithms.createTopicAreaInfo(area, "createAttachment"));

        assertEquals(areaInfoEvent.getAddresses().getAddress().size(), 0);
    }

    @Test
    public void area_addresses_level_6() {

        AreaType areaType = new AreaType();
        areaType.setCode(10L);
        areaType.setResidentsBindRate(100);
        areaType.setGender("MALE");

        Area area = new Area();
        area.setId(100L);
        area.setMoId(1L);
        area.setDescription("Test area");
        area.setAreaType(areaType);
        area.setArchived(false);
        area.setNumber(101);
        area.setAutoAssignForAttach(true);

        Addresses addresses = new Addresses();
        addresses.setAoLevel("6");
        addresses.setRegionTeCode("teCode");
        addresses.setAreaCodeOmkTe("areaTeCode");
        addresses.setStreetOmkUm("10000");
        addresses.setStreetBtiCode("bti1;bti2");
        addresses.setPlanBtiCode("bti1;bti2;bti3");
        addresses.setPlaceBtiCode("bti1;bti2;bti3;bti4");
        addresses.setCityBtiCode("bti1;bti2;bti3;bti4;bti5");
        addresses.setAreaBtiCode("bti1;bti2;bti3;bti4;bti5;bti6");
        addresses.setL1Value("House");
        addresses.setL2Value("Building");
        addresses.setL3Value("Construction");

        AreaAddress areaAddress = new AreaAddress();
        areaAddress.setAddress(addresses);
        areaAddress.setArea(area);

        area.getAreaAddresses().add(areaAddress);

        AreaInfoEvent areaInfoEvent = areaInfoEventMapper.entityToDtoTransform(algorithms.createTopicAreaInfo(area, "createAttachment"));

        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getAolevel(), addresses.getAoLevel());
        assertNull(areaInfoEvent.getAddresses().getAddress().get(0).getOmkUmCode());
        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getOmkTeDistrictCode(), addresses.getRegionTeCode());
        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getOmkTeRegionCode(), addresses.getAreaCodeOmkTe());

        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getStreetBTI().getCode().size(), 4);

        assertNull(areaInfoEvent.getAddresses().getAddress().get(0).getHouse());
        assertNull(areaInfoEvent.getAddresses().getAddress().get(0).getBuilding());
        assertNull(areaInfoEvent.getAddresses().getAddress().get(0).getConstruction());

    }

    @Test
    public void area_addresses_level_6_1() {

        AreaType areaType = new AreaType();
        areaType.setCode(10L);
        areaType.setResidentsBindRate(100);
        areaType.setGender("MALE");

        Area area = new Area();
        area.setId(100L);
        area.setMoId(1L);
        area.setDescription("Test area");
        area.setAreaType(areaType);
        area.setArchived(false);
        area.setNumber(101);
        area.setAutoAssignForAttach(true);

        Addresses addresses = new Addresses();
        addresses.setAoLevel("6");
        addresses.setRegionTeCode("teCode");
        addresses.setAreaCodeOmkTe("areaTeCode");
        addresses.setStreetOmkUm("10000");
        addresses.setStreetBtiCode("bti1;bti2");
        addresses.setPlanBtiCode("bti1;bti2;bti3");
        addresses.setCityBtiCode("bti1;bti2;bti3;bti4;bti5");
        addresses.setAreaBtiCode("bti1;bti2;bti3;bti4;bti5;bti6");
        addresses.setL1Value("House");
        addresses.setL2Value("Building");
        addresses.setL3Value("Construction");

        AreaAddress areaAddress = new AreaAddress();
        areaAddress.setAddress(addresses);
        areaAddress.setArea(area);

        area.getAreaAddresses().add(areaAddress);

        AreaInfoEvent areaInfoEvent = areaInfoEventMapper.entityToDtoTransform(algorithms.createTopicAreaInfo(area, "createAttachment"));

        assertEquals(areaInfoEvent.getAddresses().getAddress().size(), 0);
    }

    @Test
    public void area_addresses_level_4() {

        AreaType areaType = new AreaType();
        areaType.setCode(10L);
        areaType.setResidentsBindRate(100);
        areaType.setGender("MALE");

        Area area = new Area();
        area.setId(100L);
        area.setMoId(1L);
        area.setDescription("Test area");
        area.setAreaType(areaType);
        area.setArchived(false);
        area.setNumber(101);
        area.setAutoAssignForAttach(true);

        Addresses addresses = new Addresses();
        addresses.setAoLevel("4");
        addresses.setRegionTeCode("teCode");
        addresses.setAreaCodeOmkTe("areaTeCode");
        addresses.setStreetOmkUm("10000");
        addresses.setStreetBtiCode("bti1;bti2");
        addresses.setPlanBtiCode("bti1;bti2;bti3");
        addresses.setPlaceBtiCode("bti1;bti2;bti3;bti4");
        addresses.setCityBtiCode("bti1;bti2;bti3;bti4;bti5");
        addresses.setAreaBtiCode("bti1;bti2;bti3;bti4;bti5;bti6");
        addresses.setL1Value("House");
        addresses.setL2Value("Building");
        addresses.setL3Value("Construction");

        AreaAddress areaAddress = new AreaAddress();
        areaAddress.setAddress(addresses);
        areaAddress.setArea(area);

        area.getAreaAddresses().add(areaAddress);

        AreaInfoEvent areaInfoEvent = areaInfoEventMapper.entityToDtoTransform(algorithms.createTopicAreaInfo(area, "createAttachment"));

        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getAolevel(), addresses.getAoLevel());
        assertNull(areaInfoEvent.getAddresses().getAddress().get(0).getOmkUmCode());
        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getOmkTeDistrictCode(), addresses.getRegionTeCode());
        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getOmkTeRegionCode(), addresses.getAreaCodeOmkTe());

        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getStreetBTI().getCode().size(), 5);

        assertNull(areaInfoEvent.getAddresses().getAddress().get(0).getHouse());
        assertNull(areaInfoEvent.getAddresses().getAddress().get(0).getBuilding());
        assertNull(areaInfoEvent.getAddresses().getAddress().get(0).getConstruction());

    }

    @Test
    public void area_addresses_level_4_1() {

        AreaType areaType = new AreaType();
        areaType.setCode(10L);
        areaType.setResidentsBindRate(100);
        areaType.setGender("MALE");

        Area area = new Area();
        area.setId(100L);
        area.setMoId(1L);
        area.setDescription("Test area");
        area.setAreaType(areaType);
        area.setArchived(false);
        area.setNumber(101);
        area.setAutoAssignForAttach(true);

        Addresses addresses = new Addresses();
        addresses.setAoLevel("4");
        addresses.setRegionTeCode("teCode");
        addresses.setAreaCodeOmkTe("areaTeCode");
        addresses.setStreetOmkUm("10000");
        addresses.setStreetBtiCode("bti1;bti2");
        addresses.setPlanBtiCode("bti1;bti2;bti3");
        addresses.setPlaceBtiCode("bti1;bti2;bti3;bti4");
        addresses.setAreaBtiCode("bti1;bti2;bti3;bti4;bti5;bti6");
        addresses.setL1Value("House");
        addresses.setL2Value("Building");
        addresses.setL3Value("Construction");

        AreaAddress areaAddress = new AreaAddress();
        areaAddress.setAddress(addresses);
        areaAddress.setArea(area);

        area.getAreaAddresses().add(areaAddress);

        AreaInfoEvent areaInfoEvent = areaInfoEventMapper.entityToDtoTransform(algorithms.createTopicAreaInfo(area, "createAttachment"));

        assertEquals(areaInfoEvent.getAddresses().getAddress().size(), 0);
    }

    @Test
    public void area_addresses_level_3() {

        AreaType areaType = new AreaType();
        areaType.setCode(10L);
        areaType.setResidentsBindRate(100);
        areaType.setGender("MALE");

        Area area = new Area();
        area.setId(100L);
        area.setMoId(1L);
        area.setDescription("Test area");
        area.setAreaType(areaType);
        area.setArchived(false);
        area.setNumber(101);
        area.setAutoAssignForAttach(true);

        Addresses addresses = new Addresses();
        addresses.setAoLevel("3");
        addresses.setRegionTeCode("teCode");
        addresses.setAreaCodeOmkTe("areaTeCode");
        addresses.setStreetOmkUm("10000");
        addresses.setStreetBtiCode("bti1;bti2");
        addresses.setPlanBtiCode("bti1;bti2;bti3");
        addresses.setPlaceBtiCode("bti1;bti2;bti3;bti4");
        addresses.setCityBtiCode("bti1;bti2;bti3;bti4;bti5");
        addresses.setAreaBtiCode("bti1;bti2;bti3;bti4;bti5;bti6");
        addresses.setL1Value("House");
        addresses.setL2Value("Building");
        addresses.setL3Value("Construction");

        AreaAddress areaAddress = new AreaAddress();
        areaAddress.setAddress(addresses);
        areaAddress.setArea(area);

        area.getAreaAddresses().add(areaAddress);

        AreaInfoEvent areaInfoEvent = areaInfoEventMapper.entityToDtoTransform(algorithms.createTopicAreaInfo(area, "createAttachment"));

        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getAolevel(), addresses.getAoLevel());
        assertNull(areaInfoEvent.getAddresses().getAddress().get(0).getOmkUmCode());
        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getOmkTeDistrictCode(), addresses.getRegionTeCode());
        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getOmkTeRegionCode(), addresses.getAreaCodeOmkTe());

        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getStreetBTI().getCode().size(), 6);

        assertNull(areaInfoEvent.getAddresses().getAddress().get(0).getHouse());
        assertNull(areaInfoEvent.getAddresses().getAddress().get(0).getBuilding());
        assertNull(areaInfoEvent.getAddresses().getAddress().get(0).getConstruction());

    }

    @Test
    public void area_addresses_level_3_1() {

        AreaType areaType = new AreaType();
        areaType.setCode(10L);
        areaType.setResidentsBindRate(100);
        areaType.setGender("MALE");

        Area area = new Area();
        area.setId(100L);
        area.setMoId(1L);
        area.setDescription("Test area");
        area.setAreaType(areaType);
        area.setArchived(false);
        area.setNumber(101);
        area.setAutoAssignForAttach(true);

        Addresses addresses = new Addresses();
        addresses.setAoLevel("3");
        addresses.setRegionTeCode("teCode");
        addresses.setAreaCodeOmkTe("areaTeCode");
        addresses.setStreetOmkUm("10000");
        addresses.setStreetBtiCode("bti1;bti2");
        addresses.setPlanBtiCode("bti1;bti2;bti3");
        addresses.setPlaceBtiCode("bti1;bti2;bti3;bti4");
        addresses.setCityBtiCode("bti1;bti2;bti3;bti4;bti5");
        addresses.setL1Value("House");
        addresses.setL2Value("Building");
        addresses.setL3Value("Construction");

        AreaAddress areaAddress = new AreaAddress();
        areaAddress.setAddress(addresses);
        areaAddress.setArea(area);

        area.getAreaAddresses().add(areaAddress);

        AreaInfoEvent areaInfoEvent = areaInfoEventMapper.entityToDtoTransform(algorithms.createTopicAreaInfo(area, "createAttachment"));
        assertEquals(areaInfoEvent.getAddresses().getAddress().size(), 0);
    }

    @Test
    public void area_addresses_level_2() {

        AreaType areaType = new AreaType();
        areaType.setCode(10L);
        areaType.setResidentsBindRate(100);
        areaType.setGender("MALE");

        Area area = new Area();
        area.setId(100L);
        area.setMoId(1L);
        area.setDescription("Test area");
        area.setAreaType(areaType);
        area.setArchived(false);
        area.setNumber(101);
        area.setAutoAssignForAttach(true);

        Addresses addresses = new Addresses();
        addresses.setAoLevel("2");
        addresses.setRegionTeCode("teCode");
        addresses.setAreaCodeOmkTe("areaTeCode");
        addresses.setStreetOmkUm("10000");
        addresses.setStreetBtiCode("bti1;bti2");
        addresses.setPlanBtiCode("bti1;bti2;bti3");
        addresses.setPlaceBtiCode("bti1;bti2;bti3;bti4");
        addresses.setCityBtiCode("bti1;bti2;bti3;bti4;bti5");
        addresses.setL1Value("House");
        addresses.setL2Value("Building");
        addresses.setL3Value("Construction");

        AreaAddress areaAddress = new AreaAddress();
        areaAddress.setAddress(addresses);
        areaAddress.setArea(area);

        area.getAreaAddresses().add(areaAddress);

        AreaInfoEvent areaInfoEvent = areaInfoEventMapper.entityToDtoTransform(algorithms.createTopicAreaInfo(area, "createAttachment"));

        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getAolevel(), addresses.getAoLevel());
        assertNull(areaInfoEvent.getAddresses().getAddress().get(0).getOmkUmCode());
        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getOmkTeDistrictCode(), addresses.getRegionTeCode());
        assertNull(areaInfoEvent.getAddresses().getAddress().get(0).getOmkTeRegionCode());

        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getStreetBTI().getCode().size(), 0);

        assertNull(areaInfoEvent.getAddresses().getAddress().get(0).getHouse());
        assertNull(areaInfoEvent.getAddresses().getAddress().get(0).getBuilding());
        assertNull(areaInfoEvent.getAddresses().getAddress().get(0).getConstruction());
    }


    @Test
    public void area_addresses_level_25() {

        AreaType areaType = new AreaType();
        areaType.setCode(10L);
        areaType.setResidentsBindRate(100);
        areaType.setGender("MALE");

        Area area = new Area();
        area.setId(100L);
        area.setMoId(1L);
        area.setDescription("Test area");
        area.setAreaType(areaType);
        area.setArchived(false);
        area.setNumber(101);
        area.setAutoAssignForAttach(true);

        Addresses addresses = new Addresses();
        addresses.setAoLevel("25");
        addresses.setRegionTeCode("teCode");
        addresses.setAreaCodeOmkTe("areaTeCode");
        addresses.setStreetOmkUm("10000");
        addresses.setStreetBtiCode("bti1;bti2");
        addresses.setPlanBtiCode("bti1;bti2;bti3");
        addresses.setPlaceBtiCode("bti1;bti2;bti3;bti4");
        addresses.setCityBtiCode("bti1;bti2;bti3;bti4;bti5");
        addresses.setL1Value("House");
        addresses.setL2Value("Building");
        addresses.setL3Value("Construction");

        AreaAddress areaAddress = new AreaAddress();
        areaAddress.setAddress(addresses);
        areaAddress.setArea(area);

        area.getAreaAddresses().add(areaAddress);

        AreaInfoEvent areaInfoEvent = areaInfoEventMapper.entityToDtoTransform(algorithms.createTopicAreaInfo(area, "createAttachment"));

        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getAolevel(), addresses.getAoLevel());
        assertNull(areaInfoEvent.getAddresses().getAddress().get(0).getOmkUmCode());
        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getOmkTeDistrictCode(), addresses.getRegionTeCode());
        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getOmkTeRegionCode(), addresses.getAreaCodeOmkTe());

        assertEquals(areaInfoEvent.getAddresses().getAddress().get(0).getStreetBTI().getCode().size(), 0);

        assertNull(areaInfoEvent.getAddresses().getAddress().get(0).getHouse());
        assertNull(areaInfoEvent.getAddresses().getAddress().get(0).getBuilding());
        assertNull(areaInfoEvent.getAddresses().getAddress().get(0).getConstruction());
    }


}
