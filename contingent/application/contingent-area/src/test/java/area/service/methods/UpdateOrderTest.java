package area.service.methods;

import moscow.ptnl.contingent.area.entity.area.AddressAllocationOrders;
import moscow.ptnl.contingent.area.AreaErrorReason;
import moscow.ptnl.contingent.error.ContingentException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.AdditionalAnswers;
import org.mockito.ArgumentCaptor;
import service.BaseTest;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.*;

public class UpdateOrderTest extends BaseTest {

    private List<AddressAllocationOrders> orders = new ArrayList<>();
    private AddressAllocationOrders order;
    private LocalDate now;

    @BeforeEach
    public void init() {
        now = LocalDate.now();
        order = new AddressAllocationOrders();
        order.setId(2L);
        order.setDate(LocalDate.now());
        order.setName("name");
        order.setNumber("2");
        order.setOuz("");
        order.setArchived(false);
        order.setCreateDate(LocalDateTime.now());
        orders.add(new AddressAllocationOrders());
        lenient().doNothing().when(historyService).write(any(), any(), any(), any(), any(), any());
        lenient().doReturn(Optional.of(order)).when(addressAllocationOrderCRUDRepository).findById(2L);
    }

    /**
     * С_УУ_47
     */
    @Test
    public void updateOrderSUU47Test() {
        doReturn(Collections.emptyList()).when(addressAllocationOrderRepository).findAddressAllocationOrders("2", now.plusDays(4), "ouz", "name", false);
        Throwable exception = assertThrows(ContingentException.class, () -> areaServiceInternal.updateOrder(2L, "2", now.plusDays(4), "ouz", "name"));
        assertEquals(exception.getMessage(), "Дата издания распоряжения не может быть меньше 01.01.1970 или больше текущей даты");
    }

    /**
     * С_УУ_98
     */
    @Test
    public void updateOrderSUU98Test() {
        doReturn(orders).when(addressAllocationOrderRepository).findAddressAllocationOrders("3", now, "ouz", "name", false);
        Throwable exception = assertThrows(ContingentException.class, () -> areaServiceInternal.updateOrder(2L, "3", now, "ouz", "name"));
        assertEquals(exception.getMessage(), "Распоряжение с указанными параметрами уже существует в системе");
    }

    /**
     * С_УУ_99
     */
    @Test
    public void updateOrderSUU99Test() {
        doReturn(Optional.empty()).when(addressAllocationOrderCRUDRepository).findById(1L);
        Throwable exception = assertThrows(ContingentException.class, () -> areaServiceInternal.updateOrder(1L, "3", now, "ouz", "name"));
        assertEquals(exception.getMessage(), "Распоряжение с ИД 1 не найдено в системе");
    }

    /**
     * С_УУ_100
     */
    @Test
    public void updateOrderSUU100Test() {
        order.setArchived(true);
        Throwable exception = assertThrows(ContingentException.class, () -> areaServiceInternal.updateOrder(2L, "3", now, "ouz", "name"));
        assertEquals(exception.getMessage(), "Распоряжение с ИД 2 находится в архиве");
    }

    /**
     * п.5.
     */
    @Test
    public void updateOrder5Test() {
        doReturn(Collections.emptyList()).when(addressAllocationOrderRepository).findAddressAllocationOrders("23", now.minusDays(1), "ouz2", "name2", false);
        doAnswer(AdditionalAnswers.returnsFirstArg()).when(addressAllocationOrderCRUDRepository).save(any());
        ArgumentCaptor<AddressAllocationOrders> argument = ArgumentCaptor.forClass(AddressAllocationOrders.class);
        assertDoesNotThrow(() -> areaServiceInternal.updateOrder(2L, "23", now.minusDays(1), "ouz2", "name2"));
        verify(addressAllocationOrderCRUDRepository).save(argument.capture());
        assertEquals("name2", argument.getValue().getName());
        assertEquals("ouz2", argument.getValue().getOuz());
        assertEquals("23", argument.getValue().getNumber());
        assertEquals(now.minusDays(1), argument.getValue().getDate());
        assertNotNull(argument.getValue().getCreateDate());
    }

    /**
     * п.3.
     */
    @Test
    public void updateOrder6Test() {
        Throwable exception = assertThrows(ContingentException.class, () ->
                areaServiceInternal.updateOrder(2L, null, null, null, null));
        assertEquals(exception.getMessage(), AreaErrorReason.NOTHING_TO_CHANGE.getDescription());
    }

    /**
     * п.4.
     */
    @Test
    public void updateOrder7Test() {
        Throwable exception = assertThrows(ContingentException.class, () ->
                areaServiceInternal.updateOrder(2L, "2", LocalDate.now(), "", "name"));
        assertEquals(exception.getMessage(), AreaErrorReason.NOTHING_TO_CHANGE.getDescription());
    }
}
