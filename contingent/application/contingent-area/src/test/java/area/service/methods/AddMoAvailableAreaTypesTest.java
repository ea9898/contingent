package area.service.methods;

import moscow.ptnl.contingent.nsi.domain.area.AreaType;
import moscow.ptnl.contingent.domain.AreaErrorReason;
import moscow.ptnl.contingent.error.ContingentException;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

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
public class AddMoAvailableAreaTypesTest extends MoAvailableAreaTypesTest {

    /**
     * Тест п.1 С_УУ_2
     */
    @Test
    public void checkAndGetAreaTypesExistTest() {
        doReturn(Optional.of(areaType1)).when(areaTypesRepository).findById(areaType1.getCode());
        doReturn(Optional.of(areaType2)).when(areaTypesRepository).findById(areaType2.getCode());
        doReturn(Optional.of(areaType3)).when(areaTypesRepository).findById(areaType3.getCode());

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

    // Тест п.2 С_УУ_74
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

    // Тест п.3 записи в БД типов участков доступный для МО
    @Test
    public void addMoAvailableAreaTypesTest() {
        doReturn(Optional.of(areaType1)).when(areaTypesRepository).findById(areaType1.getCode());
        doReturn(Optional.of(areaType2)).when(areaTypesRepository).findById(areaType2.getCode());
        doReturn(Arrays.asList(moAvailableAreaType3, moAvailableAreaType4))
                .when(moAvailableAreaTypesRepository).findAreaTypes(moId);
        doReturn(moAvailableAreaType1).when(moAvailableAreaTypesRepository).save(moAvailableAreaType1);
        doReturn(moAvailableAreaType2).when(moAvailableAreaTypesRepository).save(moAvailableAreaType2);
        try {
            moMuService.addMoAvailableAreaTypes(moId, Arrays.asList(areaType1.getCode(),areaType2.getCode()));
        } catch (ContingentException e) {
            fail();
        }
    }

    @Test
    public void addMoAvailableAreaTypesThrowTest() {
        doReturn(Optional.of(areaType1)).when(areaTypesRepository).findById(areaType1.getCode());
        doReturn(Arrays.asList(moAvailableAreaType1, moAvailableAreaType2))
                .when(moAvailableAreaTypesRepository).findAreaTypes(moId);
        try {
            moMuService.addMoAvailableAreaTypes(moId, Arrays.asList(areaType1.getCode(),areaType2.getCode()));
        } catch (ContingentException e) {
            assertEquals(String.format(AreaErrorReason.AREA_TYPE_NOT_FOUND.getDescription(), areaType2.getCode())
                    , e.getValidation().getMessages().get(0).getMessage());
            assertEquals(String.format(AreaErrorReason.AREA_TYPE_ALREADY_EXISTS.getDescription(), areaType1.getTitle())
                    , e.getValidation().getMessages().get(1).getMessage());

        }
    }
}
