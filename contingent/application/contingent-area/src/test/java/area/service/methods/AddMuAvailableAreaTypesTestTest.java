package area.service.methods;

import moscow.ptnl.contingent.domain.area.entity.MoAvailableAreaTypes;
import moscow.ptnl.contingent.domain.area.entity.MuAvailableAreaTypes;
import moscow.ptnl.contingent.error.ContingentException;
import moscow.ptnl.contingent.error.Validation;
import moscow.ptnl.contingent.error.ValidationMessage;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

/**
 * (К_УУ_4)	Добавление типов, доступных для МУ
 */
public class AddMuAvailableAreaTypesTestTest extends MuAvailableAreaTypesTest {
    /**
     * С_УУ_73
     */
    @Test
    public void test_C_YY_73_1() {
        Mockito.doReturn(new ArrayList<>()).when(moAvailableAreaTypesRepository).findAreaTypes(2L);
        Mockito.doReturn(new ArrayList<>()).when(muAvailableAreaTypesRepository).findAreaTypes(3L);

        Mockito.doReturn(Optional.of(areaType10)).when(areaTypesCRUDRepository).findById(10L);
        Mockito.doReturn(Optional.of(areaType20)).when(areaTypesCRUDRepository).findById(20L);

        try {
            moMuService.addMuAvailableAreaTypes(2L, 3L, Arrays.asList(10L, 20L));
        } catch (ContingentException e) {
            Validation validation = e.getValidation();
            List<ValidationMessage> validationMessageList = e.getValidation().getMessages();
            if (validationMessageList.size() != 2) {
                Assertions.fail("Должно отсутствовать 2 типа участков.");
            }
            if (!validationMessageList.stream().allMatch(vm -> vm.getCode().equals("E026"))) {
                Assertions.fail("Должна быть ошибка только E026.");
            }
            if (validationMessageList.stream().map(ValidationMessage::getParameters).noneMatch(vp -> vp.get(0).getValue().equals("Тестовый тип участка 10"))) {
                Assertions.fail("Должна быть ошибка с типом участка 10.");
            }
            if (validationMessageList.stream().map(ValidationMessage::getParameters).noneMatch(vp -> vp.get(0).getValue().equals("Тестовый тип участка 20"))) {
                Assertions.fail("Должна быть ошибка с типом участка 20.");
            }
            return;
        }
        Assertions.fail("Должна быть ошибка С_УУ_73.");
    }

    @Test
    public void test_C_YY_73_2() {
        Mockito.doReturn(new ArrayList<>()).when(moAvailableAreaTypesRepository).findAreaTypes(2L);
        Mockito.doReturn(new ArrayList<>()).when(muAvailableAreaTypesRepository).findAreaTypes(3L);

        Mockito.doReturn(Optional.of(areaType10)).when(areaTypesCRUDRepository).findById(10L);
//        Mockito.doReturn(Optional.of(areaType20)).when(areaTypesCRUDRepository).findById(20L);

        try {
            moMuService.addMuAvailableAreaTypes(2L, 3L, Collections.singletonList(10L));
        } catch (ContingentException e) {
            List<ValidationMessage> validationMessageList = e.getValidation().getMessages();
            if (validationMessageList.size() != 1) {
                Assertions.fail("Должно отсутствовать 1 тип участков.");
            }
            if (!validationMessageList.stream().allMatch(vm -> vm.getCode().equals("E026"))) {
                Assertions.fail("Должна быть ошибка только E026.");
            }
            if (validationMessageList.stream().map(ValidationMessage::getParameters).noneMatch(vp -> vp.get(0).getValue().equals("Тестовый тип участка 10"))) {
                Assertions.fail("Должна быть ошибка с типом участка 10.");
            }
            return;
        }
        Assertions.fail("Должна быть ошибка С_УУ_73.");
    }

    /**
     * С_УУ_74
     */
    @Test
    public void test_C_YY_74_1() {
        Mockito.doReturn(Optional.of(areaType10)).when(areaTypesCRUDRepository).findById(10L);

        MoAvailableAreaTypes moAvailableAreaTypes10 = new MoAvailableAreaTypes();
        moAvailableAreaTypes10.setAreaType(areaType10);
        moAvailableAreaTypes10.setId(1L);
        moAvailableAreaTypes10.setCreateDate(LocalDateTime.now());
        moAvailableAreaTypes10.setMoId(2L);

        Mockito.doReturn(Collections.singletonList(moAvailableAreaTypes10)).when(moAvailableAreaTypesRepository).findAreaTypes(2L);

        MuAvailableAreaTypes muAvailableAreaTypes = new MuAvailableAreaTypes();
        muAvailableAreaTypes.setId(1L);
        muAvailableAreaTypes.setMuId(3L);
        muAvailableAreaTypes.setAreaType(areaType10);
        muAvailableAreaTypes.setCreateDate(LocalDateTime.now());
        muAvailableAreaTypes.setMoAvailableAreaType(moAvailableAreaTypes10);

        Mockito.doReturn(Collections.singletonList(muAvailableAreaTypes)).when(muAvailableAreaTypesRepository).findAreaTypes(3L);

        try {
            moMuService.addMuAvailableAreaTypes(2L, 3L, Collections.singletonList(10L));
        } catch (ContingentException e) {
            Validation validation = e.getValidation();
            if (validation.getMessages().size() != 1) {
                Assertions.fail("Должна быть 1 ошибка.");
            }
            if (!validation.getMessages().get(0).getCode().equals("E027")) {
                Assertions.fail("Должна быть ошибка E027.");
            }
            if (validation.getMessages().stream().map(ValidationMessage::getParameters).noneMatch(vp -> vp.get(0).getValue().equals("Тестовый тип участка 10"))) {
                Assertions.fail("Должна быть ошибка с типом участка 10.");
            }
            return;
        }
        Assertions.fail("Должна быть ошибка С_УУ_74.");
    }

    /**
     * п. 3.
     */
    @Test
    public void test_correct() {
        Mockito.doReturn(Optional.of(areaType10)).when(areaTypesCRUDRepository).findById(10L);
        MoAvailableAreaTypes moAvailableAreaTypes10 = new MoAvailableAreaTypes();
        moAvailableAreaTypes10.setAreaType(areaType10);
        moAvailableAreaTypes10.setId(1L);
        moAvailableAreaTypes10.setCreateDate(LocalDateTime.now());
        moAvailableAreaTypes10.setMoId(2L);

        Mockito.doReturn(Collections.singletonList(moAvailableAreaTypes10)).when(moAvailableAreaTypesRepository).findAreaTypes(2L);

        try {
            moMuService.addMuAvailableAreaTypes(2L, 3L, Collections.singletonList(10L));
        } catch (ContingentException e) {
            Assertions.fail("Сценарий корректный, ошибки быть не должно.");
        }
    }
}
