package moscow.ptnl.contingent.area.service;

import moscow.ptnl.contingent.area.entity.area.Area;
import moscow.ptnl.contingent.area.entity.area.AreaAddress;
import moscow.ptnl.contingent.area.entity.area.MoAddress;
import moscow.ptnl.contingent.area.entity.nsi.AddressFormingElement;
import moscow.ptnl.contingent.area.entity.nsi.AreaType;
import moscow.ptnl.contingent.area.error.AreaErrorReason;
import moscow.ptnl.contingent.area.error.ContingentException;
import moscow.ptnl.contingent.area.error.Validation;
import moscow.ptnl.contingent.area.model.area.Address4Algoritm;
import moscow.ptnl.contingent.area.model.area.AddressLevelType;
import moscow.ptnl.contingent.area.model.area.AddressWrapper;
import moscow.ptnl.contingent.area.model.area.NotNsiAddress;
import moscow.ptnl.contingent.area.model.area.NsiAddress;
import moscow.ptnl.contingent.repository.area.AddressesCRUDRepository;
import moscow.ptnl.contingent.repository.area.AreaAddressRepository;
import moscow.ptnl.contingent.repository.area.MoAddressRepository;
import moscow.ptnl.contingent.repository.nsi.AddressFormingElementRepository;
import moscow.ptnl.contingent.area.transform.model.XMLGregorianCalendarMapper;
import moscow.ptnl.contingent.area.transform.model.esu.AreaInfoEventMapper;
import moscow.ptnl.contingent.area.transform.model.esu.AttachOnAreaChangeMapper;
import moscow.ptnl.contingent2.area.info.AreaInfoEvent;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;


import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.stream.Collectors;
import moscow.ptnl.contingent.domain.esu.event.AttachOnAreaChangeEvent;
import moscow.ptnl.contingent2.attachment.changearea.event.AttachOnAreaChange;


/**
 * Это класс с алгоритмами А_УУ_хх
 */

@Component
public class Algorithms {

    @Autowired
    private MoAddressRepository moAddressRepository;

    @Autowired
    private AddressesCRUDRepository addressesCRUDRepository;

    @Autowired
    private AddressFormingElementRepository addressFormingElementRepository;

    @Autowired
    private AlgorithmsHelper algorithmsHelper;

    @Autowired
    private XMLGregorianCalendarMapper xmlGregorianCalendarMapper;

    @Autowired
    private AreaInfoEventMapper areaInfoEventMapper;

    @Autowired
    private AttachOnAreaChangeMapper attachOnAreaChangeMapper;

    @Autowired
    private AreaAddressRepository areaAddressRepository;

    // Поиск территорий обслуживания МО по адресу (А_УУ_1)
    public MoAddress searchServiceDistrictMOByAddress (
            Long moId,
            AreaType areaType,
            Long orderId,
            List<NsiAddress> nsiAddressList,
            List<NotNsiAddress> notNsiAddressList, Validation validation) throws ContingentException {

        // 1.
        List<MoAddress> moAddresses = moAddressRepository.getActiveMoAddresses(areaType);

        // 2.
        List<Address4Algoritm> address4Algoritms =  moAddresses.stream().map(MoAddress::getAddress)
                .map(Address4Algoritm::new).collect(Collectors.toList());


        // 3.
        List<AddressWrapper> addressWrappers = algorithmsHelper.createAfeBrList(address4Algoritms);

        // 4.
        List<AddressWrapper> addressWrapperList = findIntersectingAddresses(addressWrappers, nsiAddressList, notNsiAddressList);

        // 5.
        MoAddress serviceDestriceMo = null;
        if (addressWrapperList != null && !addressWrapperList.isEmpty()) {
            serviceDestriceMo = moAddresses.get(0);
        }

        // 6.
        return serviceDestriceMo;
    }

    // Поиск участков по адресу (А_УУ_2)
    public Long searchAreaByAddress(
            Long moId,
            AreaType areaTypeCode,
            List<NsiAddress> nsiAddressList,
            List<NotNsiAddress> notNsiAddressList) throws ContingentException {

        // 1.
        List<AreaAddress> areaAddresses = areaAddressRepository.getActiveAreaAddresses(moId, areaTypeCode.getCode());

        if (areaAddresses.isEmpty()) {
            return null;
        }

        // 2.
        List<Address4Algoritm> address4Algoritms = areaAddresses.stream().map(Address4Algoritm::new).collect(Collectors.toList());

        // 3.
        List<AddressWrapper> addressWrappers = algorithmsHelper.createAfeBrList(address4Algoritms);

        // 4.
        List<AddressWrapper> crossAddresses = findIntersectingAddresses(addressWrappers, nsiAddressList, notNsiAddressList);

        // 5.
        if (!crossAddresses.isEmpty()) {
            return areaAddressRepository.findAreaAddressByAddress(crossAddresses.get(0).getAddress()).get(0).getArea().getId();
        } else {
            return null;
        }
    }


    // Поиск пересекающихся адресов (А_УУ_3)
    public List<AddressWrapper> findIntersectingAddresses(List<AddressWrapper> afeAndBr, List<NsiAddress> nsiAddresses, List<NotNsiAddress> notNsiAddresses) throws ContingentException {
        List<AddressWrapper> intersectingAddresses = new ArrayList<>();

        List<AddressWrapper> crossAddresses = new ArrayList<>();

        // А_УУ_3 1. - 7.
        for (NsiAddress nsiAddress: nsiAddresses) {
            AddressFormingElement addressFormingElement =
                    addressFormingElementRepository.findAfeByGlobalId(nsiAddress.getGlobalId());

            if (addressFormingElement != null) {

                if (nsiAddress.getLevelAddress().equals(AddressLevelType.ID.getLevel())) {
                    crossAddresses = AlgorithmsHelper.searchById.apply(addressFormingElement, afeAndBr);
                }

                if (nsiAddress.getLevelAddress().equals(AddressLevelType.STREET.getLevel())) {
                    crossAddresses = AlgorithmsHelper.searchByStreetId.apply(addressFormingElement, afeAndBr);
                }

                if (nsiAddress.getLevelAddress().equals(AddressLevelType.PLAN.getLevel())) {
                    crossAddresses = AlgorithmsHelper.searchByPlanId.apply(addressFormingElement, afeAndBr);
                }

                if (nsiAddress.getLevelAddress().equals(AddressLevelType.PLACE.getLevel())) {
                    crossAddresses = AlgorithmsHelper.searchByPlaceId.apply(addressFormingElement, afeAndBr);
                }

                if (nsiAddress.getLevelAddress().equals(AddressLevelType.CITY.getLevel())) {
                    crossAddresses = AlgorithmsHelper.searchByCityId.apply(addressFormingElement, afeAndBr);
                }

                if (nsiAddress.getLevelAddress().equals(AddressLevelType.AREA.getLevel())) {
                    crossAddresses = AlgorithmsHelper.searchByAreaId.apply(addressFormingElement, afeAndBr);
                }

                if (nsiAddress.getLevelAddress().equals(AddressLevelType.AREA_TE.getLevel())) {
                    crossAddresses = AlgorithmsHelper.searchByAreaTeId.apply(addressFormingElement, afeAndBr);
                }
            }

            if (crossAddresses == null || !crossAddresses.isEmpty()) {
                break;
            }
        }
        if (crossAddresses == null) {
            throw new ContingentException(AreaErrorReason.INCORRECT_ADDRESS_NESTING);
        }

        if (!crossAddresses.isEmpty()) {
            return crossAddresses;
        }

        // 8.
        // 8.	Если во входных параметрах передан «Адрес вне справочника», то в перечне адресов AFE+BR выполняет поиск адресов, удовлетворяющих условию:
        //•	L1_VALUE = Дом;
        //•	L2_VALUE = Корпус;
        //•	L3_VALUE = Строение;
        //•	ADDR_ID = ИД вышестоящего элемента.
        for (NotNsiAddress notNsiAddress: notNsiAddresses) {
            crossAddresses = afeAndBr.stream()
                    .filter(aw ->aw.getBuildingRegistry() != null &&
                            aw.getBuildingRegistry().getL1Value().equals(notNsiAddress.getHouse()) &&
                                    aw.getBuildingRegistry().getL2Value().equals(notNsiAddress.getConstruction()) &&
                                    aw.getBuildingRegistry().getL3Value().equals(notNsiAddress.getBuilding()) &&
                                    aw.getBuildingRegistry().getAddrId().equals(notNsiAddress.getParentId())).collect(Collectors.toList());

            // Если найдена хотя бы одна запись найдена, то алгоритм возвращает перечень найденных адресов.
            if (!crossAddresses.isEmpty()) {
                break;
            }

            // Иначе в перечне адресов AFE выполняет поиск адресов, удовлетворяющих условиям:
            //•	GLOBAL_ID = ИД адреса вышестоящего элемента;
            //•	AOLEVEL = Уровень адреса вышестоящего элемента.
            List<AddressFormingElement> addressFormingElements = addressFormingElementRepository
                    .findAfeByIdAndLevel(notNsiAddress.getParentId(), notNsiAddress.getLevelParentId());

            // Если найдена хотя бы одна запись найдена, то алгоритм возвращает перечень найденных адресов.
            if (!addressFormingElements.isEmpty()) {
                return addressFormingElements.stream().map(AddressWrapper::new).collect(Collectors.toList());
            } else {
                // По parentId ищется AFE
                AddressFormingElement addressFormingElement =
                        addressFormingElementRepository.findAfeByGlobalId(notNsiAddress.getParentId());

                List<AddressWrapper> crossNotNsiAddresses = null;
                if (notNsiAddress.getLevelParentId().equals(AddressLevelType.STREET.getLevel())) {
                    // Если уровень вышестоящего элемента = 7, то выполняется алгоритм, описанный на шаге 1, начиная с п.  1.1, b.2.
                    crossNotNsiAddresses = AlgorithmsHelper.checkPlanIdExist.apply(addressFormingElement, afeAndBr);
                } else {
                    // Иначе выполняется алгоритм, описанный на шаге 1, начиная с п.  1.1, b.2.2.
                    crossNotNsiAddresses = AlgorithmsHelper.checkPlaceIdExist.apply(addressFormingElement, afeAndBr);
                }
                if (crossNotNsiAddresses != null && !crossNotNsiAddresses.isEmpty()) {
                    crossAddresses.addAll(crossNotNsiAddresses);
                }

                if (!crossAddresses.isEmpty()) {
                    break;
                }
            }
        }

        return crossAddresses;
    }

    //  Формирование топика «Создание или закрытие прикреплений при изменении участка» (А_УУ_4)
    public AttachOnAreaChange createTopicCreateCloseAttachAreaChange(
            List<Long> primaryAreasIdCreateAttachments,
            List<Long> primaryAreasIdCloseAttachments,
            Area dependentArea) {
        if (primaryAreasIdCloseAttachments != null && !primaryAreasIdCloseAttachments.isEmpty()) {
            if (primaryAreasIdCreateAttachments != null && !primaryAreasIdCreateAttachments.isEmpty()) {
                throw new IllegalArgumentException("Нельзя одновременно передавать primaryAreasIdCreateAttachments и primaryAreasIdCloseAttachments");
            }
            return attachOnAreaChangeMapper.entityToDtoTransform(new AttachOnAreaChangeEvent(
                    AttachOnAreaChangeEvent.OperationType.CLOSE, dependentArea, new HashSet<>(primaryAreasIdCloseAttachments)));
        }
        if (primaryAreasIdCreateAttachments != null && !primaryAreasIdCreateAttachments.isEmpty()) {
            return attachOnAreaChangeMapper.entityToDtoTransform(new AttachOnAreaChangeEvent(
                    AttachOnAreaChangeEvent.OperationType.CREATE, dependentArea, new HashSet<>(primaryAreasIdCreateAttachments)));
        }
        return null;
    }

    // Формирование топика «Сведения об участке» (А_УУ_5)
    public AreaInfoEvent createTopicAreaInfo(Area area, String methodName) {
        return areaInfoEventMapper.entityToDtoTransform(new moscow.ptnl.contingent.domain.esu.event.AreaInfoEvent(methodName, area));
    }
}
