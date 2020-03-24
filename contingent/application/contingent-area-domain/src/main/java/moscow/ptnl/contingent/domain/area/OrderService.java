package moscow.ptnl.contingent.domain.area;

import moscow.ptnl.contingent.domain.area.entity.AddressAllocationOrders;
import moscow.ptnl.contingent.error.ContingentException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;

import java.time.LocalDate;

public interface OrderService {

    /**
     * (К_УУ_18) Создание распоряжения
     * @param number
     * @param date
     * @param ouz
     * @param name
     * @return
     * @throws ContingentException
     */
    Long createOrder(String number, LocalDate date, String ouz, String name) throws ContingentException;

    /**
     * (К_УУ_19) Изменение распоряжения
     * @param id
     * @param number
     * @param date
     * @param ouz
     * @param name
     * @throws ContingentException
     */
    void updateOrder(Long id, String number, LocalDate date, String ouz, String name) throws ContingentException;

    /**
     * (К_УУ_20) Поиск распоряжений
     * @param id
     * @param number
     * @param date
     * @param name
     * @param paging
     * @return
     * @throws ContingentException
     */
    Page<AddressAllocationOrders> searchOrder(Long id, String number, LocalDate date, String name, PageRequest paging) throws ContingentException;

}
