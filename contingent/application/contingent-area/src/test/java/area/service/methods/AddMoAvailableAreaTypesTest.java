package service.methods;

import moscow.ptnl.contingent.area.entity.area.MoAvailableAreaTypes;
import moscow.ptnl.contingent.area.entity.nsi.AreaType;
import moscow.ptnl.contingent.area.error.AreaErrorReason;
import moscow.ptnl.contingent.area.error.ContingentException;
import moscow.ptnl.contingent.area.error.Validation;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import service.BaseTest;

import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;
import static org.mockito.Mockito.doReturn;

/**
 * Тест (К_УУ_1) Добавление типов участков, доступных для МО
 */
public class AddMoAvailableAreaTypesTest extends BaseTest {

    private AreaType areaType1;
    private AreaType areaType2;
    private AreaType areaType3;
    private AreaType areaType4;
    private Long moId = 204L;
    private LocalDateTime createDate = LocalDateTime.now();
    private MoAvailableAreaTypes moAvailableAreaType1;
    private MoAvailableAreaTypes moAvailableAreaType2;
    private MoAvailableAreaTypes moAvailableAreaType3;
    private MoAvailableAreaTypes moAvailableAreaType4;
    private Validation validation;


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
        validation = new Validation();
    }

    /**
     * Тест п.1 С_УУ_2
     */
    @Test
    public void checkAndGetAreaTypesExistTest() {
        doReturn(Optional.of(areaType1)).when(areaTypesCRUDRepository).findById(areaType1.getCode());
        doReturn(Optional.of(areaType2)).when(areaTypesCRUDRepository).findById(areaType2.getCode());
        doReturn(Optional.of(areaType3)).when(areaTypesCRUDRepository).findById(areaType3.getCode());

        List<AreaType> areaTypes1 = areaHelper.checkAndGetAreaTypesExist(
                Arrays.asList(areaType1.getCode(), areaType2.getCode()), validation);
        assertEquals(Arrays.asList(areaType1, areaType2), areaTypes1);
        assertTrue(validation.isSuccess());

        List<AreaType> areaTypes2 = areaHelper.checkAndGetAreaTypesExist(
                Arrays.asList(areaType1.getCode(), areaType2.getCode(), areaType3.getCode()), validation);
        assertEquals(Arrays.asList(areaType1, areaType2), areaTypes2);
        assertTrue(!validation.isSuccess());
        assertEquals(String.format(AreaErrorReason.AREA_TYPE_NOT_FOUND.getDescription(), areaType3.getCode())
                , validation.getMessages().get(0).getMessage());
    }

    /**
     * Тест п.2 С_УУ_74
     */
    @Test
    public void checkAreaTypesExistInMOTest() {
        doReturn(Arrays.asList(moAvailableAreaType1, moAvailableAreaType2))
                .when(moAvailableAreaTypesRepository).findAreaTypes(moId);
        areaHelper.checkAreaTypesExistInMO(moId, Arrays.asList(areaType3, areaType4), validation);
        assertTrue(validation.isSuccess());

        areaHelper.checkAreaTypesExistInMO(moId, Arrays.asList(areaType1, areaType2), validation);
        assertFalse(validation.isSuccess());
        assertEquals(2, validation.getMessages().size());
        assertEquals(String.format(AreaErrorReason.AREA_TYPE_ALREADY_EXISTS.getDescription(), areaType1.getTitle())
                , validation.getMessages().get(0).getMessage());
        assertEquals(String.format(AreaErrorReason.AREA_TYPE_ALREADY_EXISTS.getDescription(), areaType2.getTitle())
                , validation.getMessages().get(1).getMessage());
    }

    /**
     * Тест п.3 записи в БД типов участков доступный для МО
     */
    @Test
    public void addMoAvailableAreaTypesTest() {
        doReturn(Optional.of(areaType1)).when(areaTypesCRUDRepository).findById(areaType1.getCode());
        doReturn(Optional.of(areaType2)).when(areaTypesCRUDRepository).findById(areaType2.getCode());
        doReturn(Arrays.asList(moAvailableAreaType3, moAvailableAreaType4))
                .when(moAvailableAreaTypesRepository).findAreaTypes(moId);
        doReturn(moAvailableAreaType1).when(moAvailableAreaTypesCRUDRepository).save(moAvailableAreaType1);
        doReturn(moAvailableAreaType2).when(moAvailableAreaTypesCRUDRepository).save(moAvailableAreaType2);
        try {
            areaServiceInternal.addMoAvailableAreaTypes(moId, Arrays.asList(areaType1.getCode(),areaType2.getCode()));
        } catch (ContingentException e) {
            fail();
        }
    }

    @Test
    public void addMoAvailableAreaTypesThrowTest() {
        doReturn(Optional.of(areaType1)).when(areaTypesCRUDRepository).findById(areaType1.getCode());
        doReturn(Arrays.asList(moAvailableAreaType1, moAvailableAreaType2))
                .when(moAvailableAreaTypesRepository).findAreaTypes(moId);
        try {
            areaServiceInternal.addMoAvailableAreaTypes(moId, Arrays.asList(areaType1.getCode(),areaType2.getCode()));
        } catch (ContingentException e) {
            assertEquals(String.format(AreaErrorReason.AREA_TYPE_NOT_FOUND.getDescription(), areaType2.getCode())
                    , e.getValidation().getMessages().get(0).getMessage());
            assertEquals(String.format(AreaErrorReason.AREA_TYPE_ALREADY_EXISTS.getDescription(), areaType1.getTitle())
                    , e.getValidation().getMessages().get(1).getMessage());

        }
    }

}
