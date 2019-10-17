package moscow.ptnl.contingent.area.service;

import moscow.ptnl.contingent.area.entity.area.Addresses;
import moscow.ptnl.contingent.area.entity.area.Area;
import moscow.ptnl.contingent.area.entity.area.AreaAddress;
import moscow.ptnl.contingent.area.entity.area.MoAddress;
import moscow.ptnl.contingent.area.error.AreaErrorReason;
import moscow.ptnl.contingent.area.error.ErrorReasonImpl;
import moscow.ptnl.contingent.area.model.area.AddressLevelType;
import moscow.ptnl.contingent.area.transform.model.esu.AreaInfoEventMapper;
import moscow.ptnl.contingent.area.transform.model.esu.AttachOnAreaChangeMapper;
import moscow.ptnl.contingent.domain.esu.event.AttachOnAreaChangeEvent;
import moscow.ptnl.contingent.error.Validation;
import moscow.ptnl.contingent.error.ValidationParameter;
import moscow.ptnl.contingent.nsi.domain.area.AreaType;
import moscow.ptnl.contingent.nsi.repository.AddressFormingElementRepository;
import moscow.ptnl.contingent.repository.area.AddressesRepository;
import moscow.ptnl.contingent.repository.area.AreaAddressRepository;
import moscow.ptnl.contingent.repository.area.MoAddressRepository;
import moscow.ptnl.contingent2.area.info.AreaInfoEvent;
import moscow.ptnl.contingent2.attachment.changearea.event.AttachOnAreaChange;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import ru.mos.emias.contingent2.address.AddressRegistryBaseType;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
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

    @Autowired
    private AddressesRepository addressesRepository;

    public Algorithms() {
        super();
    }

    public Algorithms(AlgorithmsHelper algorithmsHelper) {
        this.algorithmsHelper = algorithmsHelper;
    }

    // Поиск территорий обслуживания МО по адресу (А_УУ_1)
    public MoAddress searchServiceDistrictMOByAddress(Long moId, AreaType areaType, Long orderId,
                                                      List<AddressRegistryBaseType> addressRegistryTypes, Validation validation) {

        // 1.
        List<MoAddress> moAddresses = moAddressRepository.getActiveMoAddresses(areaType);
        if (moAddresses.isEmpty()) {
            return null;
        }

        // 2.
        List<Addresses> moAddressesObj = moAddresses.stream().map(MoAddress::getAddress)
                .collect(Collectors.toList());


        // 3.
        List<Addresses> intersectingAddresses = findIntersectingAddressesAdd(addressRegistryTypes, moAddressesObj, validation);

        // 4. // 5.
        if (intersectingAddresses != null && !intersectingAddresses.isEmpty()) {
            return moAddresses.stream().filter(moAddress -> intersectingAddresses.contains(moAddress.getAddress()))
                    .findFirst().orElse(null);
        }

        return null;
    }

    // Поиск участков по адресу (А_УУ_2)
    public Long searchAreaByAddress(Long moId, AreaType areaTypeCode, List<AddressRegistryBaseType> addressRegistryTypes,
                                    Validation validation) {

        // 1.
        List<AreaAddress> areaAddresses = areaAddressRepository.getActiveAreaAddresses(moId, areaTypeCode.getCode());

        if (areaAddresses.isEmpty()) {
            return null;
        }

        // 2.
        List<Addresses> areaAddressesObj = areaAddresses.stream().map(AreaAddress::getAddress).collect(Collectors.toList());

        // 3.
        List<Addresses> intersectingAddresses = findIntersectingAddressesAdd(addressRegistryTypes, areaAddressesObj, validation);

        // 4. //5.
        if (!intersectingAddresses.isEmpty()) {
            AreaAddress address = areaAddresses.stream().filter(areaAddress -> areaAddress.getAddress().equals(intersectingAddresses.get(0)))
                    .findFirst().orElse(null);
            return address == null || address.getArea() == null ? null : address.getArea().getId();
        }
        return null;
    }


    // Поиск пересекающихся адресов (А_УУ_3)
    public List<Addresses> findIntersectingAddressesAdd(List<AddressRegistryBaseType> addressRegistryTypes,
                                                        List<Addresses> addresses, Validation validation) {

        List<Addresses> crossAddresses = new ArrayList<>();

        // 1.
        addressesRepository.findAddresses(addressRegistryTypes.stream()
                .map(AddressRegistryBaseType::getGlobalIdNsi).collect(Collectors.toList()))
                .forEach(crossAddresses::add);

        if (!crossAddresses.isEmpty()) {
            return crossAddresses;
        }

        // А_УУ_3 2. - 9.
        for (AddressRegistryBaseType addressRegistry : addressRegistryTypes) {

            if (addressRegistry.getAoLevel().equals(AddressLevelType.ID.getLevel())) {
                crossAddresses = AlgorithmsHelper.checkStreetCodeExist.apply(addressRegistry, addresses);
            }

            if (addressRegistry.getAoLevel().equals(AddressLevelType.STREET.getLevel())) {
                crossAddresses = AlgorithmsHelper.searchByStreetCode.apply(addressRegistry, addresses);
            }

            if (addressRegistry.getAoLevel().equals(AddressLevelType.PLAN.getLevel())) {
                crossAddresses = AlgorithmsHelper.searchByPlanCode.apply(addressRegistry, addresses);
            }

            if (addressRegistry.getAoLevel().equals(AddressLevelType.PLACE.getLevel())) {
                crossAddresses = AlgorithmsHelper.searchByPlaceCode.apply(addressRegistry, addresses);
            }

            if (addressRegistry.getAoLevel().equals(AddressLevelType.CITY.getLevel())) {
                crossAddresses = AlgorithmsHelper.searchByCityCode.apply(addressRegistry, addresses);
            }

            if (addressRegistry.getAoLevel().equals(AddressLevelType.AREA.getLevel())) {
                crossAddresses = AlgorithmsHelper.searchByAreaCode.apply(addressRegistry, addresses);
            }

            if (addressRegistry.getAoLevel().equals(AddressLevelType.AREA_TE.getLevel())) {
                crossAddresses = AlgorithmsHelper.searchByAreaOmkTeCode.apply(addressRegistry, addresses);
            }

            if (crossAddresses == null || !crossAddresses.isEmpty()) {
                break;
            }
        }
        if (crossAddresses == null) {
            validation.error(AreaErrorReason.INCORRECT_ADDRESS_NESTING);
        }
        //TODO непонятное условие. не имеет смысла и может вызвать NPE
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

    // Форматно-логический контроль адреса (А_УУ_6)
    public void checkAddressFLK(List<AddressRegistryBaseType> addresses, Validation validation) {
        addresses.forEach(address -> {
            String codesNotSetError = AreaErrorReason.CODES_NOT_SET.getDescription();
            //1
            if (address.getAoLevel() == null) {
                validation.error(AreaErrorReason.AO_LEVEL_NOT_SET);
            } else {
                //2
                if (address.getAoLevel().equals(AddressLevelType.MOSCOW.getLevel())) {
                    validation.error(AreaErrorReason.INCORRECT_ADDRESS_LEVEL);
                }
                //4
                if (!address.getAoLevel().equals(AddressLevelType.REGION_TE.getLevel())
                        && (address.getAreaOMKTE() == null || address.getAreaOMKTE().getCode() == null
                        || address.getAreaOMKTE().getCode().length() == 0)) {
                    codesNotSetError += " код района Москвы;";
                    //5
                } else if (address.getAoLevel().equals(AddressLevelType.AREA.getLevel())
                        && (address.getArea() == null || address.getArea().getCode() == null
                        || address.getArea().getCode().length() == 0)) {
                    codesNotSetError += " код района;";
                    //6
                } else if (address.getAoLevel().equals(AddressLevelType.CITY.getLevel())
                        && (address.getCity() == null || address.getCity().getCode() == null
                        || address.getCity().getCode().length() == 0)) {
                    codesNotSetError += " код города;";
                    //7
                } else if (address.getAoLevel().equals(AddressLevelType.PLACE.getLevel())
                        && (address.getPlace() == null || address.getPlace().getCode() == null
                        || address.getPlace().getCode().length() == 0)) {
                    codesNotSetError += " код населенного пункта;";
                    //8
                } else if (address.getAoLevel().equals(AddressLevelType.PLAN.getLevel())
                        && (address.getPlan() == null || address.getPlan().getCode() == null
                        || address.getPlan().getCode().length() == 0)) {
                    codesNotSetError += " код планировочной структуры;";
                    //9
                } else if (address.getAoLevel().equals(AddressLevelType.STREET.getLevel())
                        && (address.getStreet() == null || address.getStreet().getCode() == null
                        || address.getStreet().getCode().length() == 0)) {
                    codesNotSetError += " код код улицы;";
                    //10
                } else if (address.getAoLevel().equals(AddressLevelType.ID.getLevel())) {
                    if (address.getBuilding() == null || (
                            address.getBuilding().getHouse() == null || address.getBuilding().getHouse().getName() == null
                                    || address.getBuilding().getHouse().getName().length() == 0)
                            && (address.getBuilding().getBuild() == null || address.getBuilding().getBuild().getName() == null
                            || address.getBuilding().getBuild().getName().length() == 0)
                            && (address.getBuilding().getConstruction() == null || address.getBuilding().getConstruction().getName() == null
                            || address.getBuilding().getConstruction().getName().length() == 0)) {
                        codesNotSetError += " код дома или корпуса или строения;";
                    }
                    if (address.getRegionOMKTE() != null && address.getRegionOMKTE().getCode().split(";").length > 1
                            || address.getAreaOMKTE() != null && address.getAreaOMKTE().getCode().split(";").length > 1) {
                        validation.error(AreaErrorReason.NOT_SINGLE_AREA_OR_REGION);
                    }
                }
            }
            //3
            if (address.getRegionOMKTE() == null
                    || address.getRegionOMKTE().getCode() == null
                    || address.getRegionOMKTE().getCode().length() == 0) {
                codesNotSetError += " код округа;";
            }

            //Агрегированная ошибка С_УУ_108
            if (codesNotSetError.length() != AreaErrorReason.CODES_NOT_SET.getDescription().length()) {
                validation.error(new ErrorReasonImpl(codesNotSetError, AreaErrorReason.CODES_NOT_SET.getCode())
                        , new ValidationParameter("globalId", address.getGlobalIdNsi())
                        , new ValidationParameter("aoLevel", address.getAoLevel()));
            }
        });
    }

    // Поиск пересекающихся адресов при поиске  участков (А_УУ_7)
    // алгоритм изменён, вместо входного списка адресов НСИ, делаем запросы в нси с нужными фильтрами
    public List<Addresses> findIntersectingAddressesSearch(List<AddressRegistryBaseType> addressesRegistryType) {

        List<Long> inputIds = addressesRegistryType.stream()
                .map(AddressRegistryBaseType::getGlobalIdNsi).collect(Collectors.toList());

        Set<Addresses> resultAddresses = new HashSet<>(addressesRepository.findActualAddresses(inputIds));

        for (AddressRegistryBaseType address : addressesRegistryType) {
            switch (AddressLevelType.find(address.getAoLevel())) {
                case ID:
                case STREET:
                    if (address.getStreet() != null && address.getStreet().getCode() != null) {
                        resultAddresses.addAll(addressesRepository.findActualAddresses(address.getStreet().getCode(),
                                null, null, null, null, null, null));
                    }
                case PLAN:
                    if (address.getPlan() != null && address.getPlan().getCode() != null) {
                        resultAddresses.addAll(addressesRepository.findActualAddresses(null,
                                address.getPlan().getCode(), null, null, null, null, null));
                    }
                case PLACE:
                    if (address.getPlace() != null && address.getPlace().getCode() != null) {
                        resultAddresses.addAll(addressesRepository.findActualAddresses(null,
                                null, address.getPlace().getCode(), null, null, null, null));
                    }
                case CITY:
                    if (address.getCity() != null && address.getCity().getCode() != null) {
                        resultAddresses.addAll(addressesRepository.findActualAddresses(null,
                                null, null, address.getCity().getCode(), null, null, null));
                    }
                case AREA:
                    if (address.getArea() != null && address.getArea().getCode() != null) {
                        resultAddresses.addAll(addressesRepository.findActualAddresses(null,
                                null, null, null, address.getArea().getCode(), null, null));
                    }
                case AREA_TE:
                    if (address.getAreaOMKTE() != null && address.getAreaOMKTE().getCode() != null) {
                        List<String> areaOmkTeCodes = Arrays.asList(address.getAreaOMKTE().getCode().split(";"));
                        resultAddresses.addAll(addressesRepository.findActualAddresses(null,
                                null, null, null, null, areaOmkTeCodes, null));
                    }
                case REGION_TE:
                    if (address.getRegionOMKTE() != null && address.getRegionOMKTE().getCode() != null) {
                        List<String> regionTeCodes = Arrays.asList(address.getRegionOMKTE().getCode().split(";"));
                        resultAddresses.addAll(addressesRepository.findActualAddresses(null,
                                null, null, null, null, null, regionTeCodes));
                    }

            }
        }
        return new ArrayList<>(resultAddresses);
    }

}
