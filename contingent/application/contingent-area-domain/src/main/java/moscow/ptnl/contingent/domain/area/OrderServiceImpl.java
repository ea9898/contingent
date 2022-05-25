package moscow.ptnl.contingent.domain.area;

import moscow.ptnl.contingent.domain.AreaErrorReason;
import moscow.ptnl.contingent.domain.area.entity.AddressAllocationOrders;
import moscow.ptnl.contingent.domain.area.heplers.AreaHelper;
import moscow.ptnl.contingent.domain.area.repository.AddressAllocationOrderRepository;
import moscow.ptnl.contingent.error.ContingentException;
import moscow.ptnl.contingent.error.Validation;
import moscow.ptnl.contingent.error.ValidationParameter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Objects;

@Service
@Transactional
public class OrderServiceImpl implements OrderService {

    @Autowired
    private AddressAllocationOrderRepository addressAllocationOrderRepository;

    @Autowired
    @Lazy
    private HistoryServiceHelper historyHelper;

    @Autowired
    private AreaHelper areaHelper;

    @Override
    public Long createOrder(String number, LocalDate date, String ouz, String name) throws ContingentException {
        Validation validation = new Validation();
        areaHelper.checkDateTillToday(date, validation);

        if (!addressAllocationOrderRepository.findAddressAllocationOrders(number, date, ouz, name, false).isEmpty()) {
            validation.error(AreaErrorReason.ADDRESS_ALLOCATION_ORDER_EXISTS);
        }
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        AddressAllocationOrders order = AddressAllocationOrders.builder()
                .createDate(LocalDateTime.now())
                .updateDate(LocalDateTime.now())
                .archived(false)
                .number(number)
                .date(date)
                .ouz(ouz)
                .name(name)
                .build();

        addressAllocationOrderRepository.save(order);

        historyHelper.sendHistory(null, order, AddressAllocationOrders.class);

        return order.getId();
    }

    @Override
    public void updateOrder(Long id, String number, LocalDate date, String ouz, String name) throws ContingentException {
        Validation validation = new Validation();

        // 1.
        AddressAllocationOrders order = addressAllocationOrderRepository.findById(id).orElse(null);

        // 2.
        if (order == null) {
            validation.error(AreaErrorReason.ADDRESS_ALLOCATION_ORDER_NOT_EXISTS,
                    new ValidationParameter("orderId", id));
            throw new ContingentException(validation);
        }
        if (Boolean.TRUE.equals(order.getArchived())) {
            validation.error(AreaErrorReason.ADDRESS_ALLOCATION_ORDER_IS_ARCHIVED,
                    new ValidationParameter("orderId", id));
        }
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        AddressAllocationOrders oldOrder = historyHelper.clone(order);

        // 3.
        if (number == null && date == null && ouz == null && name == null) {
            throw new ContingentException(AreaErrorReason.NOTHING_TO_CHANGE);
        }

        // 4.
        if (Objects.deepEquals(order.getNumber(), number) &&
                Objects.deepEquals(order.getDate(), date) &&
                Objects.deepEquals(order.getOuz(), ouz) &&
                Objects.deepEquals(order.getName(), name)) {
            throw new ContingentException(AreaErrorReason.NOTHING_TO_CHANGE);
        }

        //5
        if (date != null) {
            areaHelper.checkDateTillToday(date, validation);
        }

        //6
        String numberNew = number == null ? order.getNumber() : number;
        LocalDate dateNew = date == null ? order.getDate() : date;
        String ouzNew = ouz == null ? order.getOuz() : ouz;
        String nameNew = name == null ? order.getName() : name;

        if (addressAllocationOrderRepository.findAddressAllocationOrders(numberNew, dateNew, ouzNew, nameNew, false).stream()
                .anyMatch(o -> !Objects.equals(o.getId(), id))) {
            validation.error(AreaErrorReason.ADDRESS_ALLOCATION_ORDER_EXISTS);
        }
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        //7
        order.setUpdateDate(LocalDateTime.now());
        order.setNumber(numberNew);
        order.setDate(dateNew);
        order.setOuz(ouzNew);
        order.setName(nameNew);

        addressAllocationOrderRepository.save(order);

        historyHelper.sendHistory(oldOrder, order, AddressAllocationOrders.class);

    }

    @Override
    public Page<AddressAllocationOrders> searchOrder(Long id, String number, LocalDate date, String name, PageRequest paging) throws ContingentException {
        Validation validation = new Validation();

        if (id == null && number == null && date == null && name == null) {
            validation.error(AreaErrorReason.NO_SEARCH_PARAMETERS);
            throw new ContingentException(validation);
        }
        return addressAllocationOrderRepository.findAddressAllocationOrdersOverlapped(id, number, date, name, paging);
    }
}
