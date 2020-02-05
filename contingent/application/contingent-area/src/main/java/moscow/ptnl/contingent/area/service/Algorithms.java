package moscow.ptnl.contingent.area.service;

import static moscow.ptnl.contingent.area.model.area.AddressLevelType.*;

import moscow.ptnl.contingent.area.entity.area.Addresses;
import moscow.ptnl.contingent.area.entity.area.Area;
import moscow.ptnl.contingent.area.entity.area.AreaAddress;
import moscow.ptnl.contingent.area.entity.area.MoAddress;
import moscow.ptnl.contingent.repository.SysopCRUDRepository;
import moscow.ptnl.contingent.sysop.entity.Sysop;
import moscow.ptnl.contingent.area.AreaErrorReason;
import moscow.ptnl.contingent.area.model.area.AddressLevelType;
import moscow.ptnl.contingent.area.model.sysop.SysopMethodType;
import moscow.ptnl.contingent.area.transform.SearchAreaAddress;
import moscow.ptnl.contingent.area.transform.model.esu.AreaInfoEventMapper;
import moscow.ptnl.contingent.area.transform.model.esu.AttachOnAreaChangeMapper;
import moscow.ptnl.contingent.domain.esu.event.AttachOnAreaChangeEvent;
import moscow.ptnl.contingent.error.ContingentException;
import moscow.ptnl.contingent.error.CustomErrorReason;
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
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.StringUtils;
import ru.mos.emias.contingent2.address.AddressBaseType;
import ru.mos.emias.contingent2.address.AddressRegistryBaseType;

import java.time.LocalDateTime;
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

    @Autowired
    private SysopCRUDRepository sysopCRUDRepository;

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

        // 1.
        List<Addresses> crossAddresses = addresses.stream().filter(addr ->
                addressRegistryTypes.stream().map(AddressBaseType::getGlobalIdNsi).collect(Collectors.toList()).contains(addr.getGlobalId()))
                .collect(Collectors.toList());

        if (!crossAddresses.isEmpty()) {
            return crossAddresses;
        }

        // А_УУ_3 2. - 9.
        for (AddressRegistryBaseType addressRegistry : addressRegistryTypes) {

            if (addressRegistry.getAoLevel().equals(AddressLevelType.ID.getLevel())) {
                crossAddresses = AlgorithmsHelper.checkStreetCodeExist.apply(addressRegistry, addresses);
            }

            if (addressRegistry.getAoLevel().equals(AddressLevelType.STREET.getLevel())) {
                    List<Addresses> outAddresses = addresses.stream().filter(addr ->
                            AlgorithmsHelper.streetCodeFilter.test(addressRegistry, addr))
                            .collect(Collectors.toList());
                    if (!outAddresses.isEmpty()) {
                        return outAddresses;
                    } else {
                        return AlgorithmsHelper.checkPlanCodeExist.apply(addressRegistry, addresses);
                    }
            }

            if (addressRegistry.getAoLevel().equals(AddressLevelType.PLAN.getLevel())) {
                List<Addresses> outAddresses = addresses.stream().filter(addr ->
                        AlgorithmsHelper.planCodeFilter.test(addressRegistry, addr))
                        .collect(Collectors.toList());
                if (!outAddresses.isEmpty()) {
                    return outAddresses;
                } else {
                    return AlgorithmsHelper.checkPlaceCodeExist.apply(addressRegistry, addresses);
                }
            }

            if (addressRegistry.getAoLevel().equals(AddressLevelType.PLACE.getLevel())) {
                List<Addresses> outAddresses = addresses.stream().filter(addr ->
                        AlgorithmsHelper.placeCodeFilter.test(addressRegistry, addr))
                        .collect(Collectors.toList());
                if (!outAddresses.isEmpty()) {
                    return outAddresses;
                } else {
                    return AlgorithmsHelper.checkCityCodeExist.apply(addressRegistry, addresses);
                }
            }

            if (addressRegistry.getAoLevel().equals(AddressLevelType.CITY.getLevel())) {
                List<Addresses> outAddresses = addresses.stream().filter(addr ->
                        AlgorithmsHelper.cityCodeFilter.test(addressRegistry, addr))
                        .collect(Collectors.toList());
                if (!outAddresses.isEmpty()) {
                    return outAddresses;
                } else {
                    return AlgorithmsHelper.checkAreaCodeExist.apply(addressRegistry, addresses);
                }
            }

            if (addressRegistry.getAoLevel().equals(AddressLevelType.AREA.getLevel())) {
                List<Addresses> outAddresses = addresses.stream().filter(addr ->
                        AlgorithmsHelper.areaCodeFilter.test(addressRegistry, addr))
                        .collect(Collectors.toList());
                if (!outAddresses.isEmpty()) {
                    return outAddresses;
                } else {
                    return AlgorithmsHelper.checkAreaOmkTeCodeExist.apply(addressRegistry, addresses);
                }
            }

            if (addressRegistry.getAoLevel().equals(AddressLevelType.AREA_TE.getLevel())) {
                String[] codes = addressRegistry.getAreaOMKTE().getCode().split(AlgorithmsHelper.ADDRESS_CODE_VALUES_SPLITTER);
                List<Addresses> outAddresses = addresses.stream().filter(
                        addr -> Arrays.stream(codes).anyMatch(c -> addr.getAreaCodeOmkTe() != null && addr.getAreaCodeOmkTe().equals(c)))
                        .collect(Collectors.toList());
                if (!outAddresses.isEmpty()) {
                    return outAddresses;
                } else {
                    if (addressRegistry.getRegionOMKTE() != null) {
                        String[] codes2 = addressRegistry.getRegionOMKTE().getCode().split(AlgorithmsHelper.ADDRESS_CODE_VALUES_SPLITTER);
                        return addresses.stream().filter(addr ->
                                        Arrays.stream(codes2).anyMatch(c -> addr.getRegionTeCode() != null && addr.getRegionTeCode().equals(c)))
                                .collect(Collectors.toList());
                    } else {
                        return new ArrayList<>();
                    }
                }
            }

            if (addressRegistry.getAoLevel().equals(AddressLevelType.REGION_TE.getLevel())) {
                String[] codesRegion = addressRegistry.getRegionOMKTE().getCode().split(AlgorithmsHelper.ADDRESS_CODE_VALUES_SPLITTER);
                return addresses.stream().filter(addr ->
                        Arrays.stream(codesRegion).anyMatch(c -> addr.getRegionTeCode() != null && addr.getRegionTeCode().equals(c)))
                        .collect(Collectors.toList());
            }

            if (crossAddresses == null || !crossAddresses.isEmpty()) {
                break;
            }
        }
        if (crossAddresses == null) {
            validation.error(AreaErrorReason.INCORRECT_ADDRESS_NESTING);
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
        AreaInfoEvent areaInfoEvent = areaInfoEventMapper.entityToDtoTransform(new moscow.ptnl.contingent.domain.esu.event.AreaInfoEvent(methodName, area));
        return areaInfoEvent;
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
                AddressLevelType addressLevelType = AddressLevelType.find(address.getAoLevel());
                if (addressLevelType == null || addressLevelType.getLevel().equals(AddressLevelType.MOSCOW.getLevel())) {
                    validation.error(AreaErrorReason.INCORRECT_ADDRESS_LEVEL,
                            new ValidationParameter("aoLevel", address.getAoLevel()));
                }
                //4
                if (!address.getAoLevel().equals(AddressLevelType.REGION_TE.getLevel())
                        && (address.getAreaOMKTE() == null || address.getAreaOMKTE().getCode() == null
                        || address.getAreaOMKTE().getCode().length() == 0)) {
                    codesNotSetError += " код района Москвы (areaOMKTE/code);";
                    //5
                } else if (address.getAoLevel().equals(AddressLevelType.AREA.getLevel())
                        && (address.getArea() == null || address.getArea().getCode() == null
                        || address.getArea().getCode().length() == 0)) {
                    codesNotSetError += " код района (area/code);";
                    //6
                } else if (address.getAoLevel().equals(AddressLevelType.CITY.getLevel())
                        && (address.getCity() == null || address.getCity().getCode() == null
                        || address.getCity().getCode().length() == 0)) {
                    codesNotSetError += " код города (city/code);";
                    //7
                } else if (address.getAoLevel().equals(AddressLevelType.PLACE.getLevel())
                        && (address.getPlace() == null || address.getPlace().getCode() == null
                        || address.getPlace().getCode().length() == 0)) {
                    codesNotSetError += " код населенного пункта (place/code);";
                    //8
                } else if (address.getAoLevel().equals(AddressLevelType.PLAN.getLevel())
                        && (address.getPlan() == null || address.getPlan().getCode() == null
                        || address.getPlan().getCode().length() == 0)) {
                    codesNotSetError += " код планировочной структуры (plan/code);";
                    //9
                } else if (address.getAoLevel().equals(AddressLevelType.STREET.getLevel())
                        && (address.getStreet() == null || address.getStreet().getCode() == null
                        || address.getStreet().getCode().length() == 0)) {
                    codesNotSetError += " код улицы (street/code);";
                    //10
                } else if (address.getAoLevel().equals(AddressLevelType.ID.getLevel())) {
                    if (address.getBuilding() == null || (
                            address.getBuilding().getHouse() == null || address.getBuilding().getHouse().getName() == null
                                    || address.getBuilding().getHouse().getName().length() == 0)
                            && (address.getBuilding().getBuild() == null || address.getBuilding().getBuild().getName() == null
                            || address.getBuilding().getBuild().getName().length() == 0)
                            && (address.getBuilding().getConstruction() == null || address.getBuilding().getConstruction().getName() == null
                            || address.getBuilding().getConstruction().getName().length() == 0)) {
                        codesNotSetError += " код дома (building/house/name) или корпуса (building/build/name) или строения (building/construction/name);";
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
                codesNotSetError += " код округа (regionOMKTE/code);";
            }

            //Агрегированная ошибка С_УУ_108
            if (codesNotSetError.length() != AreaErrorReason.CODES_NOT_SET.getDescription().length()) {
                validation.error(new CustomErrorReason(codesNotSetError, AreaErrorReason.CODES_NOT_SET.getCode())
                        , new ValidationParameter("globalId", address.getGlobalIdNsi())
                        , new ValidationParameter("aoLevel", address.getAoLevel()));
            }
        });
    }

    // Поиск пересекающихся адресов при поиске  участков (А_УУ_7)
    // алгоритм изменён, вместо входного списка адресов НСИ, делаем запросы в нси с нужными фильтрами
    public List<Addresses> findIntersectingAddressesSearch(List<SearchAreaAddress> addressesRegistryType) throws ContingentException {

        List<Long> inputIds = addressesRegistryType.stream()
                .map(SearchAreaAddress::getGlobalIdNsi).collect(Collectors.toList());

        Set<Addresses> resultAddresses = new HashSet<>(addressesRepository.findActualAddresses(inputIds));

        for (SearchAreaAddress address : addressesRegistryType) {
            AddressLevelType level = AddressLevelType.find(address.getAoLevel());
            List<String> areaOmkTeCodes = address.getAreaOMKTEcode() == null ? null : Arrays.asList(address.getAreaOMKTEcode().split(";"));
            List<String> regionTeCodes = address.getRegionOMKTEcode() == null ? null : Arrays.asList(address.getRegionOMKTEcode().split(";"));

            switch (level) {
                case ID:
                case STREET:
                    if (StringUtils.hasText(address.getStreetCode())) {
                        resultAddresses.addAll(addressesRepository.findActualAddresses(address.getStreetCode(), address.getPlanCode(),
                                address.getPlaceCode(), address.getCityCode(), address.getAreaCode(), areaOmkTeCodes, regionTeCodes, STREET.equals(level)));
                    }
                case PLAN:
                    if (StringUtils.hasText(address.getPlanCode())) {
                        resultAddresses.addAll(addressesRepository.findActualAddresses(null, address.getPlanCode(),
                                address.getPlaceCode(), address.getCityCode(), address.getAreaCode(), areaOmkTeCodes, regionTeCodes, PLAN.equals(level)));
                    }
                case PLACE:
                    if (StringUtils.hasText(address.getPlaceCode())) {
                        resultAddresses.addAll(addressesRepository.findActualAddresses(null, null,
                                address.getPlaceCode(), address.getCityCode(), address.getAreaCode(), areaOmkTeCodes, regionTeCodes, PLACE.equals(level)));
                    }
                case CITY:
                    if (StringUtils.hasText(address.getCityCode())) {
                        resultAddresses.addAll(addressesRepository.findActualAddresses(null, null,
                                null, address.getCityCode(), address.getAreaCode(), areaOmkTeCodes, regionTeCodes, CITY.equals(level)));
                    }
                case AREA:
                    if (StringUtils.hasText(address.getAreaCode())) {
                        resultAddresses.addAll(addressesRepository.findActualAddresses(null,null,
                                null, null, address.getAreaCode(), areaOmkTeCodes, regionTeCodes, AREA.equals(level)));
                    }
                case AREA_TE:
                    if (StringUtils.hasText(address.getAreaOMKTEcode())) {
                        resultAddresses.addAll(addressesRepository.findActualAddresses(null,
                                null, null, null, null, areaOmkTeCodes, null, AREA_TE.equals(level)));
                    } else {
                        throw new ContingentException(AreaErrorReason.INCORRECT_ADDRESS_NESTING);
                    }
                case REGION_TE:
                    if (StringUtils.hasText(address.getRegionOMKTEcode())) {
                        resultAddresses.addAll(addressesRepository.findActualAddresses(null,
                                null, null, null, null, null, regionTeCodes, REGION_TE.equals(level)));
                    }

            }
        }
        return new ArrayList<>(resultAddresses);
    }

    //Регистрация асинхронной операции (А_УУ_9)
    //Делаем в новой транзакции, чтобы закомитить до вызова асинхронного метода,
    // иначе иногда получали EntityNotFoundException: Unable to find Sysop
    @Transactional(propagation = Propagation.REQUIRES_NEW, rollbackFor = Exception.class)
    public long sysOperationRegistration(SysopMethodType methodType) {
        Sysop sysop = new Sysop(0, false);
        sysop.setMethodName(methodType.getValue());

        return sysopCRUDRepository.save(sysop).getId();
    }

    public void sysOperationComplete(long sysopId, boolean successful, String message) {
        Sysop sysop = sysopCRUDRepository.findById(sysopId).get();
        sysop.setProgress(100);
        sysop.setCompleted(true);
        sysop.setSuccessful(successful);
        sysop.setResult(message);
        sysop.setEndDate(LocalDateTime.now());
        sysopCRUDRepository.save(sysop);
    }

}
