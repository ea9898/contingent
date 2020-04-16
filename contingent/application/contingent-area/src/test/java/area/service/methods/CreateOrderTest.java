package area.service.methods;

import moscow.ptnl.contingent.domain.area.entity.AddressAllocationOrders;
import moscow.ptnl.contingent.error.ContingentException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.mockito.AdditionalAnswers;
import org.mockito.ArgumentCaptor;
import service.BaseTest;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

public class CreateOrderTest extends BaseTest {

    private List<AddressAllocationOrders> orders = new ArrayList<>();
    private LocalDate now;

    @BeforeEach
    public void init() {
        now = LocalDate.now();
        orders.add(new AddressAllocationOrders());
    }

    /**
     * С_УУ_47
     */
    @Test
    @Disabled
    public void createOrderSUU47Test() {
        doReturn(Collections.emptyList()).when(addressAllocationOrderRepository).findAddressAllocationOrders("2", now.plusDays(4), "ouz", "name", false);
        Throwable exception = assertThrows(ContingentException.class, () -> orderService.createOrder("2", now.plusDays(4), "ouz", "name"));
        assertEquals(exception.getMessage(), "Дата издания распоряжения не может быть меньше 01.01.1970 или больше текущей даты");
    }

    /**
     * С_УУ_98
     */
    @Test
    @Disabled
    public void createOrderSUU98Test() {
        doReturn(orders).when(addressAllocationOrderRepository).findAddressAllocationOrders("3", now, "ouz", "name", false);
        Throwable exception = assertThrows(ContingentException.class, () -> orderService.createOrder("3", now, "ouz", "name"));
        assertEquals(exception.getMessage(), "Распоряжение с указанными параметрами уже существует в системе");
    }

    /**
     * п.3.
     */
    @Test
    @Disabled
    public void createOrder3Test() {
        doAnswer(AdditionalAnswers.returnsFirstArg()).when(addressAllocationOrderCRUDRepository).save(any());
        doReturn(Collections.emptyList()).when(addressAllocationOrderRepository).findAddressAllocationOrders("2", now, "ouz", "name", false);
        ArgumentCaptor<AddressAllocationOrders> argument = ArgumentCaptor.forClass(AddressAllocationOrders.class);
        assertDoesNotThrow(() -> orderService.createOrder("2", now, "ouz", "name"));
        verify(addressAllocationOrderCRUDRepository).save(argument.capture());
        assertEquals("name", argument.getValue().getName());
        assertEquals("ouz", argument.getValue().getOuz());
        assertEquals("2", argument.getValue().getNumber());
        assertEquals(now, argument.getValue().getDate());
        assertNotNull(argument.getValue().getCreateDate());
    }
}
