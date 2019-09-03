package service.methods;

import moscow.ptnl.contingent.area.entity.area.MoAvailableAreaTypes;
import moscow.ptnl.contingent.area.entity.area.MuAvailableAreaTypes;
import moscow.ptnl.contingent.nsi.domain.area.AreaType;
import moscow.ptnl.contingent.error.Validation;
import org.junit.jupiter.api.BeforeEach;
import service.BaseTest;

import java.time.LocalDateTime;

abstract class MoAvailableAreaTypesTest  extends BaseTest {
    protected AreaType areaType1;
    protected AreaType areaType2;
    protected AreaType areaType3;
    protected AreaType areaType4;
    protected Long moId = 204L;
    protected LocalDateTime createDate = LocalDateTime.now();
    protected MoAvailableAreaTypes moAvailableAreaType1;
    protected MoAvailableAreaTypes moAvailableAreaType2;
    protected MoAvailableAreaTypes moAvailableAreaType3;
    protected MoAvailableAreaTypes moAvailableAreaType4;
    protected MuAvailableAreaTypes muAvailableAreaType1;
    protected MuAvailableAreaTypes muAvailableAreaType2;
    protected Validation validation;


    @BeforeEach
    public void init() {
        areaType1 = new AreaType(10L, "Терапевтический", false);
        areaType2 = new AreaType(20L, "Педиатрический", false);
        areaType3 = new AreaType(30L, "Взрослый стоматологический", true);
        areaType4 = new AreaType(70L, "Гинекологический", false);
        moAvailableAreaType1 = new MoAvailableAreaTypes(204L, areaType1, createDate, null);
        moAvailableAreaType2 = new MoAvailableAreaTypes(204L, areaType2, createDate, null);
        moAvailableAreaType3 = new MoAvailableAreaTypes(204L, areaType3, createDate, null);
        moAvailableAreaType4 = new MoAvailableAreaTypes(204L, areaType4, createDate, null);
        muAvailableAreaType1 = new MuAvailableAreaTypes(123L, areaType1, moAvailableAreaType1, null);
        muAvailableAreaType2 = new MuAvailableAreaTypes(129L, areaType2, moAvailableAreaType2, null);
        validation = new Validation();
    }
}
