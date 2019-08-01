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
import moscow.ptnl.contingent.area.model.area.NsiAddress;
import moscow.ptnl.contingent.area.transform.model.esu.AreaInfoEventMapper;
import moscow.ptnl.contingent.area.transform.model.esu.AttachOnAreaChangeMapper;
import moscow.ptnl.contingent.domain.esu.event.AttachOnAreaChangeEvent;
import moscow.ptnl.contingent.repository.area.AreaAddressRepository;
import moscow.ptnl.contingent.repository.area.MoAddressRepository;
import moscow.ptnl.contingent.repository.nsi.AddressFormingElementRepository;
import moscow.ptnl.contingent2.area.info.AreaInfoEvent;
import moscow.ptnl.contingent2.attachment.changearea.event.AttachOnAreaChange;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Это класс с алгоритмами А_УУ_хх
 */

@Component
public class Algorithms {

    @Autowired
    private MoAddressRepository moAddressRepository;

    @Autowired
    private AddressFormingElementRepository addressFormingElementRepository;

    @Autowired
    private AlgorithmsHelper algorithmsHelper;

    @Autowired
    private AreaInfoEventMapper areaInfoEventMapper;

    @Autowired
    private AttachOnAreaChangeMapper attachOnAreaChangeMapper;

    @Autowired
    private AreaAddressRepository areaAddressRepository;

    // Поиск территорий обслуживания МО по адресу (А_УУ_1)
    public MoAddress searchServiceDistrictMOByAddress (Long moId, AreaType areaType, Long orderId,
            List<NsiAddress> nsiAddressList, Validation validation) throws ContingentException {

        // 1.
        List<MoAddress> moAddresses = moAddressRepository.getActiveMoAddresses(areaType);

        // 2.
        List<Address4Algoritm> address4Algoritms =  moAddresses.stream().map(MoAddress::getAddress)
                .map(Address4Algoritm::new).collect(Collectors.toList());


        // 3.
        List<AddressWrapper> addressWrappers = algorithmsHelper.createAfeBrList(address4Algoritms);

        // 4.
        List<AddressWrapper> addressWrapperList = findIntersectingAddresses(addressWrappers, nsiAddressList);

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
            List<NsiAddress> nsiAddressList) throws ContingentException {

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
        List<AddressWrapper> crossAddresses = findIntersectingAddresses(addressWrappers, nsiAddressList);

        // 5.
        if (!crossAddresses.isEmpty()) {
            return areaAddressRepository.findAreaAddressByAddress(crossAddresses.get(0).getAddress()).get(0).getArea().getId();
        } else {
            return null;
        }
    }


    // Поиск пересекающихся адресов (А_УУ_3)
    public List<AddressWrapper> findIntersectingAddresses(List<AddressWrapper> afeAndBr
            , List<NsiAddress> nsiAddresses) throws ContingentException {

        List<AddressWrapper> crossAddresses = new ArrayList<>();

        // А_УУ_3 1. - 7.
        for (NsiAddress nsiAddress : nsiAddresses) {
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
        
        throw new IllegalArgumentException("недопустимые аргументы для создания топика");
    }

    // Формирование топика «Сведения об участке» (А_УУ_5)
    public AreaInfoEvent createTopicAreaInfo(Area area, String methodName) {        
        return areaInfoEventMapper.entityToDtoTransform(new moscow.ptnl.contingent.domain.esu.event.AreaInfoEvent(methodName, area));
    }
}
