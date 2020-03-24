package moscow.ptnl.contingent.domain.area;

import moscow.ptnl.contingent.domain.area.entity.Area;
import moscow.ptnl.contingent.domain.area.entity.AreaAddress;
import moscow.ptnl.contingent.domain.area.model.area.AddressArea;
import moscow.ptnl.contingent.domain.area.model.area.AreaInfo;
import moscow.ptnl.contingent.error.ContingentException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;

import java.util.List;

/**
 * Интерфейс доменного сервиса для методов работы с участками.
 */
public interface AreaService {

    /**
     * (К_УУ_12) Получение подробной информации об участке
     * Получение подробной информации об участке по ИД участка
     * @param areaId
     * @return
     * @throws ContingentException
     */
    AreaInfo getAreaById(Long areaId) throws ContingentException;

    /**
     * (К_УУ_15) Получение списка адресов участка обслуживания
     * Метод позволяет получить список адресов участков обслуживания в пределах одного МО
     * @param moId
     * @param areaIds
     * @param paging
     * @return
     * @throws ContingentException
     */
    Page<AreaAddress> getAreaAddress(Long moId, List<Long> areaIds, PageRequest paging) throws ContingentException;

    /**
     * (К_УУ_24) Получение идентификатора для создания нового участка
     * Предоставление идентификатора для создания нового участка на стороне Контингент 1
     * @return
     * @throws ContingentException
     */
    Long getNewAreaId() throws ContingentException;

    /**
     * (К_УУ_25) Предоставление списка участков
     * Просмотр списка участков обслуживания
     * @param areaTypeClassCode
     * @param moId
     * @param muIds
     * @param areaTypeCodes
     * @param number
     * @param description
     * @param isArchived
     * @param medicalEmployees
     * @param searchAreaAddresses
     * @param isExactAddressMatch
     * @param paging
     * @return
     * @throws ContingentException
     */
/*
    Page<AreaInfo> searchArea(Long areaTypeClassCode, Long moId, List<Long> muIds, List<Long> areaTypeCodes,
                              Integer number, String description, Boolean isArchived,
                              List<SearchAreaRequest.MedicalEmployee> medicalEmployees,
                              List<SearchAreaAddress> searchAreaAddresses, Boolean isExactAddressMatch,
                              PageRequest paging) throws ContingentException;
*/

    /**
     * (К_УУ_29) Получение списка участков для ДН
     * @param moId
     * @param muIds
     * @param areaTypeCodes
     * @param specializationCodes
     * @param areaIds
     * @param paging
     * @return
     * @throws ContingentException
     */
    Page<Area> searchDnArea(Long moId, List<Long> muIds, List<Long> areaTypeCodes, List<Long> specializationCodes,
                            List<Long> areaIds, PageRequest paging) throws ContingentException;
}
