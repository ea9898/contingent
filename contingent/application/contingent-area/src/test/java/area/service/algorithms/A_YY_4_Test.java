package service.algorithms;

import moscow.ptnl.contingent.domain.area.entity.Area;
import service.BaseTest;

import moscow.ptnl.contingent.domain.area.entity.AreaToAreaType;
import moscow.ptnl.contingent.nsi.domain.area.AreaType;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeClass;
import moscow.ptnl.contingent2.attachment.changearea.event.AreaRestriction;
import moscow.ptnl.contingent2.attachment.changearea.event.AttachOnAreaChange;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public class A_YY_4_Test  extends BaseTest {

    private AreaType areaTypePrimary1;
    private AreaType areaTypeDependent1;
    private AreaType areaTypeDependent2;
    private AreaToAreaType areaToAreaType1;
    private Area areaPrimary1;
    private Area areaDependent1;
    private Area areaDependent2;
    private Long moId = 204L;

    @BeforeEach
    public void init() {
        AreaTypeClass areaTypeClass1 = new AreaTypeClass();
        areaTypeClass1.setCode(1L);
        areaTypeClass1.setTitle("Primary test");
        areaTypeClass1.setArchived(false);
        AreaTypeClass areaTypeClass2 = new AreaTypeClass();
        areaTypeClass2.setCode(10L);
        areaTypeClass2.setTitle("Dependent test");
        areaTypeClass2.setArchived(false);
        areaTypePrimary1 = new AreaType(10L, "Терапевтический", false);
        areaTypePrimary1.setAreaTypeClass(areaTypeClass1);
        areaTypeDependent1 = new AreaType(20L, "Терапевтический 2", false);
        areaTypeDependent1.setAreaTypeClass(areaTypeClass2);
        areaTypeDependent2 = new AreaType(30L, "Терапевтический 3", false);
        areaTypeDependent2.setAreaTypeClass(areaTypeClass2);
        areaTypeDependent2.setAgeMin(1);
        areaTypeDependent2.setAgeMax(16);
        areaPrimary1 = Area.builder()
                .id(1L)
                .moId(moId)
                .areaType(areaTypePrimary1)
                .archived(false)
                .createDate(LocalDateTime.now())
                .build();                
        areaDependent1 = Area.builder()
                .id(2L)
                .moId(moId)
                .areaType(areaTypeDependent1)
                .archived(false)
                .createDate(LocalDateTime.now())
                .ageMin(1)
                .ageMax(16)
                .build();
        areaDependent2 = Area.builder()
                .id(3L)
                .moId(moId)
                .areaType(areaTypeDependent2)
                .archived(false)
                .createDate(LocalDateTime.now())
                .build();                
        areaToAreaType1 = new AreaToAreaType();
        areaToAreaType1.setId(1L);
        areaToAreaType1.setArea(areaDependent1);
        areaToAreaType1.setAreaType(areaTypeDependent1);
    }

    @Test
    public void topicMapCreateWithAreaRestrictionsTest() {
        List<Long> primaryAreaTypeCodesIds = Arrays.asList(areaPrimary1.getId());

        AttachOnAreaChange result = attachOnAreaChangeMapper.entityToDtoTransform(
                algorithms.createTopicCreateCloseAttachAreaChange(primaryAreaTypeCodesIds,
                null, areaDependent1));
        assertNotNull(result.getOperationDate());
        topicCheckAreaMap(result.getDependendArea(), areaDependent1);
        assertEquals(result.getPrimaryAreaAdd(), primaryAreaTypeCodesIds);
        assertEquals(result.getPrimaryAreaDel(), Collections.EMPTY_LIST);
    }

    @Test
    public void topicMapCloseWithAreaTypeRestrictionsTest() {
        List<Long> primaryAreaTypeCodesIds = Arrays.asList(areaPrimary1.getId());

        AttachOnAreaChange result = attachOnAreaChangeMapper.entityToDtoTransform(
                algorithms.createTopicCreateCloseAttachAreaChange(null,
                primaryAreaTypeCodesIds, areaDependent2));
        assertNotNull(result.getOperationDate());
        topicCheckAreaMap(result.getDependendArea(), areaDependent2);
        assertEquals(result.getPrimaryAreaDel(), primaryAreaTypeCodesIds);
        assertEquals(result.getPrimaryAreaAdd(), Collections.EMPTY_LIST);
    }

    private void topicCheckAreaMap(AttachOnAreaChange.DependendArea dependentArea, Area areaDependent) {
        assertNotNull(dependentArea);
        assertEquals(dependentArea.getAreaId(), (long) areaDependent.getId());
        assertEquals(dependentArea.getMoId(), (long) areaDependent.getMoId());
        assertEquals(dependentArea.getMuId(), areaDependent.getMuId());
        assertEquals(dependentArea.getPolicyType(), Arrays.asList(1L));
        assertNotNull(dependentArea.getAreaRestriction());
        AreaRestriction restriction = dependentArea.getAreaRestriction();
        boolean empty = areaDependent.getAgeMin() == null && areaDependent.getAgeMax() == null &&
                areaDependent.getAgeMMin() == null && areaDependent.getAgeWMax() == null &&
                areaDependent.getAgeWMin() == null && areaDependent.getAgeWMax() == null;
        assertEquals(restriction.getMinAge(), empty ? areaDependent.getAreaType().getAgeMin() : areaDependent.getAgeMin());
        assertEquals(restriction.getMinAgeMale(), empty ? areaDependent.getAreaType().getAgeMMin() : areaDependent.getAgeMMin());
        assertEquals(restriction.getMinAgeFemale(), empty ? areaDependent.getAreaType().getAgeWMin() : areaDependent.getAgeWMin());
        assertEquals(restriction.getMaxAge(), empty ? areaDependent.getAreaType().getAgeMax() : areaDependent.getAgeMax());
        assertEquals(restriction.getMaxAgeMale(), empty ? areaDependent.getAreaType().getAgeMMax() : areaDependent.getAgeMMax());
        assertEquals(restriction.getMaxAgeFemale(), empty ? areaDependent.getAreaType().getAgeWMax() : areaDependent.getAgeWMax());
    }

    @Test
    public void topicMapIncorrect1Test() {
        assertThrows(IllegalArgumentException.class, () -> algorithms.createTopicCreateCloseAttachAreaChange(
                Arrays.asList(areaPrimary1.getId()), Arrays.asList(areaPrimary1.getId()), areaDependent1));
    }
}
