package moscow.ptnl.contingent.domain.area;

import moscow.ptnl.contingent.domain.area.entity.MoAddress;
import moscow.ptnl.contingent.domain.area.model.area.AreaTypeStateType;
import moscow.ptnl.contingent.domain.area.model.area.MoAddressWithAddresses;
import moscow.ptnl.contingent.domain.area.model.area.MuAreaTypesFull;
import moscow.ptnl.contingent.error.ContingentException;
import moscow.ptnl.contingent.nsi.domain.area.AreaType;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;

import java.time.LocalDate;
import java.util.List;

public interface MoMuService {

    /**
     * (К_УУ_1) Добавление типов участков, доступных для МО
     * @param moId
     * @param areaTypeCodes
     * @throws ContingentException
     */
    void addMoAvailableAreaTypes(long moId, List<Long> areaTypeCodes) throws ContingentException;

    /**
     * (К_УУ_2)	Удаление типов участков из доступных для МО
     * @param moId
     * @param areaTypeCodes
     * @throws ContingentException
     */
    void delMoAvailableAreaTypes(long moId, List<Long> areaTypeCodes) throws ContingentException;

    /**
     * (К_УУ_3)	Предоставление типов участков, доступных для МО
     * @param moId
     * @return
     * @throws ContingentException
     */
    List<AreaType> getMoAvailableAreaTypes(long moId) throws ContingentException;

    void addMuAvailableAreaTypesV3(long moId, long muId, List<Long> areaTypeCodes) throws ContingentException;

    /**
     * (К_УУ_4)	Добавление типов, доступных для МУ
     * @param moId
     * @param muId
     * @param areaTypeCodes
     * @throws ContingentException
     */
    void addMuAvailableAreaTypes(long moId, long muId, List<Long> areaTypeCodes) throws ContingentException;

    /**
     * (К_УУ_5)	Удаление типов участков из доступных для МУ
     * @param muId
     * @param areaTypeCodes
     * @throws ContingentException
     */
    void delMuAvailableAreaTypes(long muId, List<Long> areaTypeCodes) throws ContingentException;

    /**
     * (К_УУ_6)	Предоставление типов участков, доступных для МУ
     * @param moId
     * @param muId
     * @param areaTypeState
     * @return
     * @throws ContingentException
     */
    MuAreaTypesFull getMuAvailableAreaTypes(long moId, long muId, AreaTypeStateType areaTypeState) throws ContingentException;

    /**
     * (К_УУ_21) Распределение жилых домов к территории обслуживания МО
     * @param moId
     * @param areaTypeCode
     * @param orderId
     * @param addressesRegistry
     * @param limitAddress
     * @return
     */
//    List<Long> addMoAddress(long moId, long areaTypeCode, long orderId, List<AddressRegistryBaseType> addressesRegistry, boolean limitAddress);

    /**
     * (К_УУ_22) Отмена распределения жилых домов к территории обслуживания МО
     * @param moAddressIds
     * @param orderId
     * @throws ContingentException
     */
    void delMoAddress(List<Long> moAddressIds, long orderId) throws ContingentException;

    /**
     * (К_УУ_23) Получение списка территорий обслуживания МО
     * @param moId
     * @param areaTypeCodes
     * @param paging
     * @return
     * @throws ContingentException
     */
    Page<MoAddress> getMoAddress(long moId, List<Long> areaTypeCodes, PageRequest paging) throws ContingentException;

    /**
     * (К_УУ_35) Отмена распределения жилых домов к территории обслуживания всех МО
     * @param orderId
     * @param addressGlobalIds
     * @return
     */
    boolean delMoAddressTotal(long orderId, List<Long> addressGlobalIds) throws ContingentException;

    /**
     * (К_УУ_34) Поиск территорий обслуживания МО по параметрам
     * @param moId
     * @param addressGlobalIds
     * @param areaTypeCodes
     * @param orderDate
     * @param orderName
     * @param orderNumber
     * @param orderOuz
     * @param orderCreateDate
     * @param paging
     * @return
     * @throws ContingentException
     */
    Page<MoAddressWithAddresses> searchMoAddress(long moId, List<Long> addressGlobalIds, List<Long> areaTypeCodes, LocalDate orderDate,
                                                 String orderName, String orderNumber, String orderOuz, LocalDate orderCreateDate,
                                                 PageRequest paging) throws ContingentException;
}
