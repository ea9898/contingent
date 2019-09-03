package service.methods;

import moscow.ptnl.contingent.area.entity.area.MoAvailableAreaTypes;
import moscow.ptnl.contingent.area.error.AreaErrorReason;
import moscow.ptnl.contingent.error.ContingentException;
import org.junit.jupiter.api.Test;

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
public class DelMoAvailableAreaTypesTest extends MoAvailableAreaTypesTest {

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
