package area.service.methods;

import moscow.ptnl.contingent.area.entity.area.MoAvailableAreaTypes;
import moscow.ptnl.contingent.area.entity.area.MuAvailableAreaTypes;
import moscow.ptnl.contingent.area.error.ContingentException;
import moscow.ptnl.contingent.area.model.area.AreaTypeStateType;
import moscow.ptnl.contingent.area.model.area.MuAreaTypesFull;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

import java.time.LocalDateTime;
import java.util.Collections;

/**
 * (К_УУ_6)	Предоставление типов участков, доступных для МУ
 */
public class GetMuAvailableAreaTypesTest extends MuAvailableAreaTypesTest {

    @BeforeEach
    @Override
    public void init() {
        super.init();

    }

    @Test
    public void test_find_all_types() {

        Mockito.doReturn(Collections.singletonList(muAvailableAreaTypes)).when(muAvailableAreaTypesRepository).findAreaTypes(muId);
        Mockito.doReturn(Collections.singletonList(moAvailableAreaTypes20)).when(moAvailableAreaTypesRepository).findAreaTypes(moId);

        MuAreaTypesFull muAreaTypesFull = null;
        try {
            muAreaTypesFull = areaServiceInternal.getMuAvailableAreaTypes(moId, muId, AreaTypeStateType.ALL);
        } catch (ContingentException e) {
            Assertions.fail("В этом методе ошибок не предусмотрено");
        }
        if (muAreaTypesFull == null || muAreaTypesFull.getAvailableAreaTypes() == null
                || muAreaTypesFull.getAvailableAreaTypes().isEmpty()  || muAreaTypesFull.getUsedAreaTypes() == null ||
                muAreaTypesFull.getUsedAreaTypes().isEmpty()) {
            Assertions.fail("Not null");
        }
    }

    @Test
    public void test_find_available_types() {

        Mockito.doReturn(Collections.singletonList(muAvailableAreaTypes)).when(muAvailableAreaTypesRepository).findAreaTypes(muId);
        Mockito.doReturn(Collections.singletonList(moAvailableAreaTypes20)).when(moAvailableAreaTypesRepository).findAreaTypes(moId);

        MuAreaTypesFull muAreaTypesFull = null;
        try {
            muAreaTypesFull = areaServiceInternal.getMuAvailableAreaTypes(moId, muId, AreaTypeStateType.AVAILABLE_TO_ADD);
        } catch (ContingentException e) {
            Assertions.fail("В этом методе ошибок не предусмотрено");
        }
        if (muAreaTypesFull == null || muAreaTypesFull.getAvailableAreaTypes() == null
                || muAreaTypesFull.getAvailableAreaTypes().isEmpty()) {
            Assertions.fail("Доступные участки должны вернуться.");
        }
        if (muAreaTypesFull.getUsedAreaTypes() != null && !muAreaTypesFull.getUsedAreaTypes().isEmpty()) {
            Assertions.fail("Используемые участки не должны вернуться.");
        }
    }

    @Test
    public void test_find_used_types() {

        Mockito.doReturn(Collections.singletonList(muAvailableAreaTypes)).when(muAvailableAreaTypesRepository).findAreaTypes(muId);

        MuAreaTypesFull muAreaTypesFull = null;
        try {
            muAreaTypesFull = areaServiceInternal.getMuAvailableAreaTypes(moId, muId, AreaTypeStateType.USED_IN_MU);
        } catch (ContingentException e) {
            Assertions.fail("В этом методе ошибок не предусмотрено");
        }
        if (muAreaTypesFull == null || muAreaTypesFull.getUsedAreaTypes() == null
                || muAreaTypesFull.getUsedAreaTypes().isEmpty()) {
            Assertions.fail("Используемые участки должны вернуться.");
        }
        if (muAreaTypesFull.getAvailableAreaTypes() != null && !muAreaTypesFull.getAvailableAreaTypes().isEmpty()) {
            Assertions.fail("Доступные участки не должны вернуться.");
        }
    }

}
