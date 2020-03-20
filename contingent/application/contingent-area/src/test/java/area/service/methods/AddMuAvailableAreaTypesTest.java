package service.methods;

import service.BaseTest;
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

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import static org.mockito.Mockito.doReturn;

/**
 * (К_УУ_4)	Добавление типов, доступных для МУ
 */
public class AddMuAvailableAreaTypesTest extends BaseTest {

    private AreaType areaType10;
    private AreaType areaType20;

    @BeforeEach
    public void init() {
        areaType10 = new AreaType(10L, "Тестовый тип участка 10", false);
        areaType20 = new AreaType(20L, "Тестовый тип участка 20", false);
    }

    /**
     * С_УУ_73
     */
    @Test
    public void test_C_YY_73_1() {
        Mockito.when(moAvailableAreaTypesRepository.findAreaTypes(2L)).thenReturn(new ArrayList<>());
        Mockito.when(muAvailableAreaTypesRepository.findAreaTypes(3L)).thenReturn(new ArrayList<>());

        Mockito.when(areaTypesCRUDRepository.findById(Mockito.anyLong()))
                .then(invocation -> {
                    Long invocationId = invocation.getArgument(0);
                    if (invocationId.equals(10L)) {
                        return Optional.of(areaType10);
                    }
                    if (invocationId.equals(20L)) {
                        return Optional.of(areaType20);
                    }
                    return Optional.empty();
                });

        try {
            areaServiceInternal.addMuAvailableAreaTypes(2L, 3L, Arrays.asList(10L, 20L));
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
        Mockito.when(moAvailableAreaTypesRepository.findAreaTypes(2L)).thenReturn(new ArrayList<>());
        Mockito.when(muAvailableAreaTypesRepository.findAreaTypes(3L)).thenReturn(new ArrayList<>());

        Mockito.when(areaTypesCRUDRepository.findById(Mockito.anyLong()))
                .then(invocation -> {
                    Long invocationId = invocation.getArgument(0);
                    if (invocationId.equals(10L)) {
                        return Optional.of(areaType10);
                    }
                    if (invocationId.equals(20L)) {
                        return Optional.of(areaType20);
                    }
                    return Optional.empty();
                });

        try {
            areaServiceInternal.addMuAvailableAreaTypes(2L, 3L, Collections.singletonList(10L));
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
        doReturn(Optional.of(areaType10)).when(areaTypesCRUDRepository).findById(areaType10.getCode());
        MoAvailableAreaTypes moAvailableAreaTypes10 = new MoAvailableAreaTypes();
        moAvailableAreaTypes10.setAreaType(areaType10);
        moAvailableAreaTypes10.setId(1L);
        moAvailableAreaTypes10.setCreateDate(LocalDateTime.now());
        moAvailableAreaTypes10.setMoId(2L);

        Mockito.when(moAvailableAreaTypesRepository.findAreaTypes(2L)).thenReturn(Collections.singletonList(moAvailableAreaTypes10));

        MuAvailableAreaTypes muAvailableAreaTypes = new MuAvailableAreaTypes();
        muAvailableAreaTypes.setId(1L);
        muAvailableAreaTypes.setMuId(3L);
        muAvailableAreaTypes.setAreaType(areaType10);
        muAvailableAreaTypes.setCreateDate(LocalDateTime.now());
        muAvailableAreaTypes.setMoAvailableAreaType(moAvailableAreaTypes10);

        Mockito.when(muAvailableAreaTypesRepository.findAreaTypes(3L)).thenReturn(Collections.singletonList(muAvailableAreaTypes));

//        Mockito.when(muAvailableAreaTypesCRUDRepository.save(muAvailableAreaTypes)).thenReturn(null);

        try {
            areaServiceInternal.addMuAvailableAreaTypes(2L, 3L, Collections.singletonList(10L));
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
        doReturn(Optional.of(areaType10)).when(areaTypesCRUDRepository).findById(areaType10.getCode());
        MoAvailableAreaTypes moAvailableAreaTypes10 = new MoAvailableAreaTypes();
        moAvailableAreaTypes10.setAreaType(areaType10);
        moAvailableAreaTypes10.setId(1L);
        moAvailableAreaTypes10.setCreateDate(LocalDateTime.now());
        moAvailableAreaTypes10.setMoId(2L);

        Mockito.when(moAvailableAreaTypesRepository.findAreaTypes(2L)).thenReturn(Collections.singletonList(moAvailableAreaTypes10));

        try {
            areaServiceInternal.addMuAvailableAreaTypes(2L, 3L, Collections.singletonList(10L));
        } catch (ContingentException e) {
            Assertions.fail("Сценарий корректный, ошибки быть не должно.");
        }
    }
}
