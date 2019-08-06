package area.service.methods;

import moscow.ptnl.contingent.area.entity.area.MoAvailableAreaTypes;
import moscow.ptnl.contingent.area.entity.area.MuAvailableAreaTypes;
import moscow.ptnl.contingent.area.entity.nsi.AreaType;
import moscow.ptnl.contingent.area.error.AreaErrorReason;
import moscow.ptnl.contingent.area.error.ContingentException;
import moscow.ptnl.contingent.area.error.Validation;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import service.BaseTest;

import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;
import static org.mockito.Mockito.doReturn;

/**
 * Тест (К_УУ_2) Удаление типов участков из доступных для МО
 */
public class DelMoAvailableAreaTypesTest extends BaseTest {

    private AreaType areaType1;
    private AreaType areaType2;
    private AreaType areaType3;
    private Long moId = 204L;
    private LocalDateTime createDate = LocalDateTime.now();
    private MoAvailableAreaTypes moAvailableAreaType1;
    private MoAvailableAreaTypes moAvailableAreaType2;
    private MuAvailableAreaTypes muAvailableAreaType1;
    private MuAvailableAreaTypes muAvailableAreaType2;
    private Validation validation;


    @BeforeEach
    public void init() {
        areaType1 = new AreaType(10L, "Терапевтический", false);
        areaType2 = new AreaType(20L, "Педиатрический", false);
        areaType3 = new AreaType(30L, "Взрослый стоматологический", true);
        moAvailableAreaType1 = new MoAvailableAreaTypes(204L, areaType1, createDate, null);
        moAvailableAreaType2 = new MoAvailableAreaTypes(204L, areaType2, createDate, null);
        muAvailableAreaType1 = new MuAvailableAreaTypes(123L, areaType1, moAvailableAreaType1, null);
        muAvailableAreaType2 = new MuAvailableAreaTypes(129L, areaType2, moAvailableAreaType2, null);
        validation = new Validation();
    }

    /**
     * Тест С_УУ_73
     */
    @Test
    public void checkAndGetAreaTypesNotExistInMOTest() {
        doReturn(Arrays.asList(moAvailableAreaType1, moAvailableAreaType2))
                .when(moAvailableAreaTypesRepository).findAreaTypes(moId);
        doReturn(Optional.of(areaType3)).when(areaTypesCRUDRepository).findById(areaType3.getCode());
        List<MoAvailableAreaTypes> moAvailableAreaTypes = areaHelper.checkAndGetAreaTypesNotExistInMO(
                moId, Arrays.asList(areaType1.getCode(), areaType2.getCode(), areaType3.getCode()), validation);
        assertEquals(Arrays.asList(moAvailableAreaType1, moAvailableAreaType2), moAvailableAreaTypes);
        assertFalse(validation.isSuccess());
        assertEquals(String.format(AreaErrorReason.AREA_TYPE_NOT_EXISTS_IN_MO.getDescription(), areaType3.getTitle())
                , validation.getMessages().get(0).getMessage());
    }

    /**
     * Тест С_УУ_77
     */
    @Test
    public void checkAndGetAreaTypesNotExistInMUTrueTest() {
        doReturn(Arrays.asList(moAvailableAreaType1, moAvailableAreaType2))
                .when(moAvailableAreaTypesRepository).findAreaTypes(moId);
        List<MoAvailableAreaTypes> moAvailableAreaTypes = areaHelper.checkAndGetAreaTypesNotExistInMO(
                moId, Arrays.asList(areaType1.getCode(), areaType2.getCode()), validation);
        areaHelper.checkAndGetAreaTypesNotExistInMU(
                moAvailableAreaTypes, Arrays.asList(
                        areaType1.getCode(), areaType2.getCode(), areaType3.getCode()), validation);
        assertTrue(validation.isSuccess());
    }

    @Test
    public void checkAndGetAreaTypesNotExistInMUFalseTest() {
        moAvailableAreaType2.setMuAvailableAreaTypes(
                new HashSet<>(Arrays.asList(muAvailableAreaType1, muAvailableAreaType2)));
        doReturn(Arrays.asList(moAvailableAreaType1, moAvailableAreaType2))
                .when(moAvailableAreaTypesRepository).findAreaTypes(moId);
        List<MoAvailableAreaTypes> moAvailableAreaTypes = areaHelper.checkAndGetAreaTypesNotExistInMO(
                moId, Arrays.asList(areaType1.getCode(), areaType2.getCode()), validation);
        areaHelper.checkAndGetAreaTypesNotExistInMU(
                moAvailableAreaTypes, Arrays.asList(
                        areaType1.getCode(), areaType2.getCode(), areaType3.getCode()), validation);
        assertFalse(validation.isSuccess());
        assertEquals(2, validation.getMessages().size());
        assertEquals(
                String.format(AreaErrorReason.CANT_DELETE_AREA_TYPE.getDescription()
                        , areaType2.getCode(), muAvailableAreaType2.getMuId())
                , validation.getMessages().get(0).getMessage());
        assertEquals(
                String.format(AreaErrorReason.CANT_DELETE_AREA_TYPE.getDescription()
                        , areaType1.getCode(), muAvailableAreaType1.getMuId())
                , validation.getMessages().get(1).getMessage());
    }


    /**
     * Тест п.3
     */
    @Test
    public void delMoAvailableAreaTypesCorrectTest() {
        doReturn(Arrays.asList(moAvailableAreaType1, moAvailableAreaType2))
                .when(moAvailableAreaTypesRepository).findAreaTypes(moId);
        try {
            areaServiceInternal.delMoAvailableAreaTypes(moId, Arrays.asList(areaType1.getCode(), areaType2.getCode()));
        } catch (ContingentException e) {
            fail();
        }
    }

    @Test
    public void delMoAvailableAreaTypesExceptionTest() {
        moAvailableAreaType2.setMuAvailableAreaTypes(
                new HashSet<>(Arrays.asList(muAvailableAreaType1, muAvailableAreaType2)));
        doReturn(Arrays.asList(moAvailableAreaType1, moAvailableAreaType2))
                .when(moAvailableAreaTypesRepository).findAreaTypes(moId);
        doReturn(Optional.of(areaType3)).when(areaTypesCRUDRepository).findById(areaType3.getCode());
        try {
            areaServiceInternal.delMoAvailableAreaTypes(
                    moId, Arrays.asList(areaType1.getCode(), areaType2.getCode(), areaType3.getCode()));
        } catch (ContingentException e) {
            assertFalse(e.getValidation().isSuccess());
            assertEquals(3, e.getValidation().getMessages().size());
            assertEquals(String.format(AreaErrorReason.AREA_TYPE_NOT_EXISTS_IN_MO.getDescription(), areaType3.getTitle())
                    , e.getValidation().getMessages().get(0).getMessage());
            assertEquals(String.format(AreaErrorReason.CANT_DELETE_AREA_TYPE.getDescription()
                    , areaType2.getCode(), muAvailableAreaType2.getMuId())
                    , e.getValidation().getMessages().get(1).getMessage());
            assertEquals(
                    String.format(AreaErrorReason.CANT_DELETE_AREA_TYPE.getDescription()
                            , areaType1.getCode(), muAvailableAreaType1.getMuId())
                    , e.getValidation().getMessages().get(2).getMessage());
        }
    }
}
