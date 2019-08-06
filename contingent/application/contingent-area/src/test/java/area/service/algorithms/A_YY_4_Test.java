package area.service.algorithms;

import moscow.ptnl.contingent.area.entity.area.Area;
import area.service.BaseTest;

import moscow.ptnl.contingent.area.entity.area.AreaToAreaType;
import moscow.ptnl.contingent.area.entity.nsi.AreaType;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeClass;
import moscow.ptnl.contingent2.attachment.changearea.event.AttachOnAreaChange;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.List;

public class A_YY_4_Test  extends BaseTest {

    private AreaType areaTypePrimary1;
    private AreaType areaTypeDependent1;
    private AreaToAreaType areaToAreaType1;
    private Area areaPrimary1;
    private Area areaDependent1;
    private Long moId = 204L;

    @BeforeEach
    public void init() {
        AreaTypeClass areaTypeClass1 = new AreaTypeClass();
        areaTypeClass1.setCode(1L);
        areaTypeClass1.setTitle("Primary test");
        areaTypeClass1.setArchived(false);
        AreaTypeClass areaTypeClass2 = new AreaTypeClass();
        areaTypeClass1.setCode(10L);
        areaTypeClass1.setTitle("Dependent test");
        areaTypeClass1.setArchived(false);
        areaTypePrimary1 = new AreaType(10L, "Терапевтический", false);
        areaTypePrimary1.setAreaTypeClass(areaTypeClass1);
        areaTypeDependent1 = new AreaType(20L, "Терапевтический 2", false);
        areaTypeDependent1.setAreaTypeClass(areaTypeClass2);
        areaPrimary1 = new Area(1L, moId, null, areaTypePrimary1, false, LocalDateTime.now());
        areaDependent1 = new Area(1L, moId, null, areaTypeDependent1, false, LocalDateTime.now());
        areaToAreaType1 = new AreaToAreaType();
        areaToAreaType1.setId(1L);
        areaToAreaType1.setArea(areaDependent1);
        areaToAreaType1.setAreaType(areaTypeDependent1);
    }

    @Test
    public void test1() {
        List<Long> primaryAreaTypeCodesIds = Arrays.asList(areaPrimary1.getId());

        AttachOnAreaChange result = algorithms.createTopicCreateCloseAttachAreaChange(primaryAreaTypeCodesIds,
                null, areaDependent1);
        assertNotNull(result);
    }
}
