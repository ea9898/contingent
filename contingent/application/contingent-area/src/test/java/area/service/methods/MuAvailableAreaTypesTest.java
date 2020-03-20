package area.service.methods;

import moscow.ptnl.contingent.area.entity.area.MoAvailableAreaTypes;
import moscow.ptnl.contingent.area.entity.area.MuAvailableAreaTypes;
import moscow.ptnl.contingent.nsi.domain.area.AreaType;
import org.junit.jupiter.api.BeforeEach;
import service.BaseTest;

import java.time.LocalDateTime;

public class MuAvailableAreaTypesTest extends BaseTest {

    protected AreaType areaType10;
    protected AreaType areaType20;
    protected Long moId = 2L;
    protected Long muId = 3L;
    protected MoAvailableAreaTypes moAvailableAreaTypes10;
    protected MoAvailableAreaTypes moAvailableAreaTypes20;
    protected MuAvailableAreaTypes muAvailableAreaTypes;
    protected MuAvailableAreaTypes muAvailableAreaTypes10;
    protected MuAvailableAreaTypes muAvailableAreaTypes20;

    @BeforeEach
    public void init() {
        areaType10 = new AreaType(10L, "Тестовый тип участка 10", false);
        areaType20 = new AreaType(20L, "Тестовый тип участка 20", false);

        moAvailableAreaTypes10 = new MoAvailableAreaTypes();
        moAvailableAreaTypes10.setAreaType(areaType10);
        moAvailableAreaTypes10.setId(1L);
        moAvailableAreaTypes10.setCreateDate(LocalDateTime.now());
        moAvailableAreaTypes10.setMoId(moId);

        moAvailableAreaTypes20 = new MoAvailableAreaTypes();
        moAvailableAreaTypes20.setAreaType(areaType10);
        moAvailableAreaTypes20.setId(1L);
        moAvailableAreaTypes20.setCreateDate(LocalDateTime.now());
        moAvailableAreaTypes20.setMoId(moId);

        muAvailableAreaTypes = new MuAvailableAreaTypes();
        muAvailableAreaTypes.setId(1L);
        muAvailableAreaTypes.setMuId(muId);
        muAvailableAreaTypes.setAreaType(areaType20);
        muAvailableAreaTypes.setCreateDate(LocalDateTime.now());
        muAvailableAreaTypes.setMoAvailableAreaType(moAvailableAreaTypes20);

        muAvailableAreaTypes20 = new MuAvailableAreaTypes();
        muAvailableAreaTypes20.setId(2L);
        muAvailableAreaTypes20.setMuId(muId);
        muAvailableAreaTypes20.setAreaType(areaType20);
        muAvailableAreaTypes20.setCreateDate(LocalDateTime.now());
        muAvailableAreaTypes20.setMoAvailableAreaType(moAvailableAreaTypes20);

        muAvailableAreaTypes10 = new MuAvailableAreaTypes();
        muAvailableAreaTypes10.setId(3L);
        muAvailableAreaTypes10.setMuId(muId);
        muAvailableAreaTypes10.setAreaType(areaType10);
        muAvailableAreaTypes10.setCreateDate(LocalDateTime.now());
        muAvailableAreaTypes10.setMoAvailableAreaType(moAvailableAreaTypes10);
    }
}
