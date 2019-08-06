package service.methods;

import moscow.ptnl.contingent.area.entity.nsi.AreaType;
import moscow.ptnl.contingent.area.error.ContingentException;
import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;
import static org.mockito.Mockito.doReturn;

/**
 *  Тест (К_УУ_3) Предоставление типов участков, доступных для МО
 */
public class GetMoAvailableAreaTypesTest extends MoAvailableAreaTypesTest {

    @Test
    public void getMoAvailableAreaTypesExistsTest() {
        doReturn(Arrays.asList(moAvailableAreaType1, moAvailableAreaType2))
                .when(moAvailableAreaTypesRepository).findAreaTypes(moId);
        try {
            List<AreaType> areaTypes = areaServiceInternal.getMoAvailableAreaTypes(moId);
            assertEquals(Arrays.asList(areaType1, areaType2), areaTypes);
        } catch (ContingentException e) {
            fail();
        }
    }

    @Test
    public void getMoAvailableAreaTypesEmptyTest() {
        try {
            List<AreaType> areaTypes = areaServiceInternal.getMoAvailableAreaTypes(moId);
            assertEquals(Collections.EMPTY_LIST, areaTypes);
        } catch (ContingentException e) {
            fail();
        }
    }
}
