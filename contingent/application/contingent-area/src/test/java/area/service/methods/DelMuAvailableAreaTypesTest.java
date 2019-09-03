package area.service.methods;

import com.google.common.collect.Lists;
import moscow.ptnl.contingent.area.entity.area.MoAvailableAreaTypes;
import moscow.ptnl.contingent.area.entity.area.MuAvailableAreaTypes;
import moscow.ptnl.contingent.nsi.domain.area.AreaType;
import moscow.ptnl.contingent.error.ContingentException;
import moscow.ptnl.contingent.error.Validation;
import moscow.ptnl.contingent.error.ValidationMessage;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import service.BaseTest;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

/**
 * (К_УУ_5)	Удаление типов участков из доступных для МУ
 */
public class DelMuAvailableAreaTypesTest extends MuAvailableAreaTypesTest {

    /**
     * С_УУ_75
     */
    @Test
    public void test_C_YY_75_1(){
        Mockito.doReturn(Optional.of(areaType20)).when(areaTypesCRUDRepository).findById(20L);

        MuAvailableAreaTypes muAvailableAreaTypes = new MuAvailableAreaTypes();
        muAvailableAreaTypes.setId(1L);
        muAvailableAreaTypes.setMuId(muId);
        muAvailableAreaTypes.setAreaType(areaType10);
        muAvailableAreaTypes.setCreateDate(LocalDateTime.now());
        muAvailableAreaTypes.setMoAvailableAreaType(moAvailableAreaTypes10);

        Mockito.doReturn(Collections.singletonList(muAvailableAreaTypes)).when(muAvailableAreaTypesRepository).findAreaTypes(muId);

        try {
            areaServiceInternal.delMuAvailableAreaTypes(3L, Arrays.asList(10L, 20L));
        } catch (ContingentException e) {
            List<ValidationMessage> validationMessages = e.getValidation().getMessages();
            if (validationMessages.size() != 1 || !validationMessages.get(0).getCode().equals("E028") ||
                    !validationMessages.get(0).getParameters().get(0).getValue().equals("Тестовый тип участка 20")){
                Assertions.fail("Должна быть 1 ошибка E028 с кодом участка 20.");
            }
            return;
        }
        Assertions.fail("Должна быть ошибка С_УУ_75.");
    }

    @Test
    public void test_C_YY_75_2(){
        Mockito.doReturn(Optional.of(areaType10)).when(areaTypesCRUDRepository).findById(10L);
        Mockito.doReturn(Optional.of(areaType20)).when(areaTypesCRUDRepository).findById(20L);

        Mockito.doReturn(new ArrayList<>()).when(muAvailableAreaTypesRepository).findAreaTypes(muId);

        try {
            areaServiceInternal.delMuAvailableAreaTypes(muId, Arrays.asList(10L, 20L));
        } catch (ContingentException e) {
            List<ValidationMessage> validationMessages = e.getValidation().getMessages();
            if (validationMessages.size() != 2 || !validationMessages.stream().allMatch(vm -> vm.getCode().equals("E028"))){
                Assertions.fail("Должна быть 1 ошибка E028 с кодом участка 20.");
            }
            return;
        }
        Assertions.fail("Должна быть ошибка С_УУ_75.");
    }

    /**
     * п.2
     */

    @Test
    public void test_correct() {

        List<MuAvailableAreaTypes> muAvailableAreaTypes = Arrays.asList(muAvailableAreaTypes10, muAvailableAreaTypes20);

        Mockito.doReturn(muAvailableAreaTypes).when(muAvailableAreaTypesRepository).findAreaTypes(muId);

        try {
            areaServiceInternal.delMuAvailableAreaTypes(muId, Arrays.asList(10L, 20L));
        } catch (ContingentException e) {
            Assertions.fail("Должна быть не должно.");
        }

        Mockito.verify(muAvailableAreaTypesCRUDRepository).deleteAll(muAvailableAreaTypes);
    }

    /**
     * Тест на уникальность участков
     */
    @Test
    public void test_correct_1() {

        List<MuAvailableAreaTypes> muAvailableAreaTypes = Arrays.asList(muAvailableAreaTypes10, muAvailableAreaTypes20);

        Mockito.doReturn(muAvailableAreaTypes).when(muAvailableAreaTypesRepository).findAreaTypes(muId);

        try {
            areaServiceInternal.delMuAvailableAreaTypes(3L, Arrays.asList(10L, 20L, 10L));
        } catch (ContingentException e) {
            Assertions.fail("Должна быть не должно.");
        }

        Mockito.verify(muAvailableAreaTypesCRUDRepository).deleteAll(muAvailableAreaTypes);
        Mockito.verify(areaHelper).checkAndGetAreaTypesNotExistInMU(Mockito.eq(3L), Mockito.eq(Arrays.asList(10L, 20L)), Mockito.any(Validation.class));
    }
}
