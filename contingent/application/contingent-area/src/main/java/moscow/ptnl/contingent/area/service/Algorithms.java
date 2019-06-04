package moscow.ptnl.contingent.area.service;

import moscow.ptnl.contingent.area.entity.area.Area;
import moscow.ptnl.contingent.area.entity.area.MoAddress;
import moscow.ptnl.contingent.area.entity.nsi.AddressFormingElement;
import moscow.ptnl.contingent.area.entity.nsi.AreaType;
import moscow.ptnl.contingent.area.entity.nsi.BuildingRegistry;
import moscow.ptnl.contingent.area.error.AreaErrorReason;
import moscow.ptnl.contingent.area.error.ContingentException;
import moscow.ptnl.contingent.area.error.Validation;
import moscow.ptnl.contingent.area.model.area.Address4Algoritm;
import moscow.ptnl.contingent.area.model.area.AddressLevelType;
import moscow.ptnl.contingent.area.model.area.AddressWrapper;
import moscow.ptnl.contingent.area.repository.area.AddressesCRUDRepository;
import moscow.ptnl.contingent.area.repository.area.MoAddressRepository;
import moscow.ptnl.contingent.area.repository.nsi.AddressFormingElementCRUDRepository;
import moscow.ptnl.contingent.area.repository.nsi.AddressFormingElementRepository;
import moscow.ptnl.contingent.area.repository.nsi.BuildingRegistryCRUDRepository;
import moscow.ptnl.contingent.area.transform.model.XMLGregorianCalendarMapper;
import moscow.ptnl.contingent2.area.info.AreaInfoEvent;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import ru.mos.emias.contingent2.core.NotNsiAddress;
import ru.mos.emias.contingent2.core.NsiAddress;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * Это класс с алгоритмами А_УУ_хх
 */

@Component
public class Algorithms {

    @Autowired
    MoAddressRepository moAddressRepository;

    @Autowired
    AddressesCRUDRepository addressesCRUDRepository;

    @Autowired
    AddressFormingElementRepository addressFormingElementRepository;

    @Autowired
    AddressFormingElementCRUDRepository addressFormingElementCRUDRepository;

    @Autowired
    BuildingRegistryCRUDRepository buildingRegistryCRUDRepository;

    @Autowired
    AlgorithmsHelper algorithmsHelper;

    @Autowired
    XMLGregorianCalendarMapper xmlGregorianCalendarMapper;



    // Поиск территорий обслуживания МО по адресу (А_УУ_1)
    public Long searchServiceDistrictMOByAddress (
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
        List<AddressWrapper> addressWrappers = new ArrayList<>();
        address4Algoritms.forEach(al -> {
            if (!AddressLevelType.ID.getLevel().equals(al.getLevel())) {
                // 3.1.
                addressWrappers.addAll(addressFormingElementRepository.findAfeByIdAndLevel(
                        al.getAddressId(), al.getLevel()).stream().map(AddressWrapper::new)
                        .collect(Collectors.toList()));
            } else {
                // 3.2.
                // a.
                Optional<BuildingRegistry> buildingRegistryOptional = buildingRegistryCRUDRepository.findById(al.getAddressId());
                if (buildingRegistryOptional.isPresent()) {
                    BuildingRegistry buildingRegistry = buildingRegistryOptional.get();
                    Long afeId = buildingRegistry.getAddressFormingElement().getId();

                    // b.
                    AddressFormingElement addressFormingElement = addressFormingElementCRUDRepository.findById(afeId).get();

                    // c.
                    AddressWrapper addressWrapper = new AddressWrapper();
                    addressWrapper.setAddressFormingElement(addressFormingElement);
                    addressWrapper.setBuildingRegistry(buildingRegistry);

                    // d.
                    addressWrapper.setAddressLevelType(AddressLevelType.ID);
                    addressWrapper.setBrGlobalId(buildingRegistry.getGlobalId());

                    addressWrappers.add(addressWrapper);
                }

            }
        });

        // 4.
        List<AddressWrapper> addressWrapperList = findIntersectingAddresses(addressWrappers, nsiAddressList, notNsiAddressList);

        // 5.
        Long serviceDestriceMoId = null;
        if (addressWrapperList != null && !addressWrapperList.isEmpty()) {
            serviceDestriceMoId = moAddresses.get(0).getId();
        }

        // 6.
        return serviceDestriceMoId;
    }

    // Поиск участков по адресу (А_УУ_2)
    public List<Long> searchAreaByAddress(
            Long moId,
            Long areaId,
            List<NsiAddress> nsiAddressList,
            List<NotNsiAddress> notNsiAddressList) {

        return new ArrayList<>();
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

                if (nsiAddress.getLevelAddress() == AddressLevelType.ID.getLevel()) {
                    crossAddresses = algorithmsHelper.searchById.apply(addressFormingElement, afeAndBr);
                }

                if (nsiAddress.getLevelAddress() == AddressLevelType.STREET.getLevel()) {
                    crossAddresses = algorithmsHelper.searchByStreetId.apply(addressFormingElement, afeAndBr);
                }

                if (nsiAddress.getLevelAddress() == AddressLevelType.PLAN.getLevel()) {
                    crossAddresses = algorithmsHelper.searchByPlanId.apply(addressFormingElement, afeAndBr);
                }

                if (nsiAddress.getLevelAddress() == AddressLevelType.PLACE.getLevel()) {
                    crossAddresses = algorithmsHelper.searchByPlaceId.apply(addressFormingElement, afeAndBr);
                }

                if (nsiAddress.getLevelAddress() == AddressLevelType.CITY.getLevel()) {
                    crossAddresses = algorithmsHelper.searchByCityId.apply(addressFormingElement, afeAndBr);
                }

                if (nsiAddress.getLevelAddress() == AddressLevelType.AREA.getLevel()) {
                    crossAddresses = algorithmsHelper.searchByAreaId.apply(addressFormingElement, afeAndBr);
                }

                if (nsiAddress.getLevelAddress() == AddressLevelType.AREA_TE.getLevel()) {
                    crossAddresses = algorithmsHelper.searchByAreaTeId.apply(addressFormingElement, afeAndBr);
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
                    .filter(aw ->
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
                if (notNsiAddress.getParentId() == AddressLevelType.STREET.getLevel()) {
                    // Если уровень вышестоящего элемента = 7, то выполняется алгоритм, описанный на шаге 1, начиная с п.  1.1, b.2.
                    crossAddresses.addAll(AlgorithmsHelper.checkPlanIdExist.apply(addressFormingElement, afeAndBr));
                } else {
                    // Иначе выполняется алгоритм, описанный на шаге 1, начиная с п.  1.1, b.2.2.
                    crossAddresses.addAll(AlgorithmsHelper.checkPlaceIdExist.apply(addressFormingElement, afeAndBr));
                }

                if (!crossAddresses.isEmpty()) {
                    break;
                }
            }
        }

        return crossAddresses;
    }




    //  Формирование топика «Создание или закрытие прикреплений при изменении участка» (А_УУ_4)
    // TODO реализовать
    public String createTopicCreateCloseAttachAreaChange() {
        return "";
    }

    // Формирование топика «Сведения об участке» (А_УУ_5)
    public String createTopicAreaInfo(Area area, String methodName) {

        // 1.
        AreaInfoEvent areaInfoEvent = new AreaInfoEvent();

        areaInfoEvent.setId(1);
        areaInfoEvent.setOperationDate(xmlGregorianCalendarMapper.entityToDtoTransform(LocalDateTime.now()));
        areaInfoEvent.setOperationType(methodName);
        areaInfoEvent.setAreaId(area.getId());
        areaInfoEvent.setAreaType(area.getAreaType().getCode());
        areaInfoEvent.setMuId(area.getMuId());
        areaInfoEvent.setNumber(area.getNumber());
        areaInfoEvent.setName(area.getDescription());
        areaInfoEvent.setArchive(area.getArchived());
        areaInfoEvent.setAutoAssignForAttachment(area.getAutoAssignForAttach());
//        areaInfoEvent.setResidentsBindRate(area.getAreaType().);

        return "";
    }
}
