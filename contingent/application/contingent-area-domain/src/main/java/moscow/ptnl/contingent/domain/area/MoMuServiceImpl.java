package moscow.ptnl.contingent.domain.area;

import moscow.ptnl.contingent.domain.AreaErrorReason;
import moscow.ptnl.contingent.domain.area.entity.AreaAddress;
import moscow.ptnl.contingent.domain.area.entity.MoAddress;
import moscow.ptnl.contingent.domain.area.entity.MoAvailableAreaTypes;
import moscow.ptnl.contingent.domain.area.entity.MuAvailableAreaTypes;
import moscow.ptnl.contingent.domain.area.entity.MuMuService;
import moscow.ptnl.contingent.domain.area.heplers.AreaHelper;
import moscow.ptnl.contingent.domain.area.model.area.AreaTypeStateType;
import moscow.ptnl.contingent.domain.area.model.area.MoAddressWithAddresses;
import moscow.ptnl.contingent.domain.area.model.area.MuAreaTypesFull;
import moscow.ptnl.contingent.domain.area.repository.AreaAddressRepository;
import moscow.ptnl.contingent.domain.area.repository.MoAddressRepository;
import moscow.ptnl.contingent.domain.area.repository.MoAvailableAreaTypesRepository;
import moscow.ptnl.contingent.domain.area.repository.MuAvailableAreaTypesRepository;
import moscow.ptnl.contingent.domain.area.repository.MuMuServiceRepository;
import moscow.ptnl.contingent.error.ContingentException;
import moscow.ptnl.contingent.error.Validation;
import moscow.ptnl.contingent.error.ValidationParameter;
import moscow.ptnl.contingent.infrastructure.service.setting.SettingService;
import moscow.ptnl.contingent.nsi.domain.area.AreaType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * Сервис для работы с медицинскими организациями.
 */

@Service
@Transactional
public class MoMuServiceImpl implements MoMuService {

    @Autowired
    private MoAvailableAreaTypesRepository moAvailableAreaTypesRepository;

    @Autowired
    private MuAvailableAreaTypesRepository muAvailableAreaTypesRepository;

    @Autowired
    private MoAddressRepository moAddressRepository;

    @Autowired
    private AreaHelper areaHelper;

    @Autowired
    private SettingService settingService;

    @Autowired
    private AreaAddressRepository areaAddressRepository;

    @Autowired
    private MuMuServiceRepository muMuServiceRepository;

    @Autowired
    @Lazy
    private HistoryServiceHelper historyHelper;

    @Override
    public void addMoAvailableAreaTypes(long moId, List<Long> areaTypeCodes) throws ContingentException {
        areaTypeCodes = areaTypeCodes.stream().distinct().collect(Collectors.toList());
        Validation validation = new Validation();
        List<AreaType> areaTypes = areaHelper.checkAndGetAreaTypesExist(areaTypeCodes, validation);
        areaHelper.checkAreaTypesExistInMO(moId, areaTypes, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        areaTypeCodes.forEach(a -> {
            MoAvailableAreaTypes availableAreaType = new MoAvailableAreaTypes();
            areaTypes.stream()
                    .filter(t -> Objects.equals(t.getCode(), a))
                    .findFirst()
                    .ifPresent(availableAreaType::setAreaType);
            availableAreaType.setMoId(moId);
            availableAreaType.setCreateDate(LocalDateTime.now());
            moAvailableAreaTypesRepository.save(availableAreaType);
        });
    }

    @Override
    public void delMoAvailableAreaTypes(long moId, List<Long> areaTypeCodes) throws ContingentException {
        areaTypeCodes = areaTypeCodes.stream().distinct().collect(Collectors.toList());
        Validation validation = new Validation();
        areaTypeCodes = areaHelper.checkAndGetAreaTypesExist(areaTypeCodes, validation).stream()
                .map(AreaType::getCode)
                .collect(Collectors.toList());
        List<MoAvailableAreaTypes> moAvailableAreaTypes = areaHelper.   checkAndGetAreaTypesNotExistInMO(moId, areaTypeCodes, validation);
        areaHelper.checkAndGetAreaTypesNotExistInMU(moAvailableAreaTypes, areaTypeCodes, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        moAvailableAreaTypes.forEach(a -> moAvailableAreaTypesRepository.delete(a));
    }

    @Override
    public List<AreaType> getMoAvailableAreaTypes(long moId) throws ContingentException {
        List<MoAvailableAreaTypes> moAvailableAreaTypes = moAvailableAreaTypesRepository.findAreaTypes(moId);
        return moAvailableAreaTypes.stream().map(MoAvailableAreaTypes::getAreaType).collect(Collectors.toList());
    }

    @Override
    public void addMuAvailableAreaTypes(long moId, long muId, List<Long> areaTypeCodes) throws ContingentException {
        areaTypeCodes = areaTypeCodes.stream().distinct().collect(Collectors.toList());
        Validation validation = new Validation();
        areaTypeCodes = areaHelper.checkAndGetAreaTypesExist(areaTypeCodes, validation).stream()
                .map(AreaType::getCode)
                .collect(Collectors.toList());
        // 1.
        List<MoAvailableAreaTypes> moAvailableAreaTypes = areaHelper.checkAndGetAreaTypesNotExistInMO(moId, areaTypeCodes, validation);
        // 2.
        areaHelper.checkAreaTypesExistInMU(muId, moAvailableAreaTypes.stream()
                .map(MoAvailableAreaTypes::getAreaType)
                .collect(Collectors.toList()), validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        // 3.
        moAvailableAreaTypes.forEach(a -> {
            MuAvailableAreaTypes muAvailableAreaType = new MuAvailableAreaTypes();
            muAvailableAreaType.setMuId(muId);
            muAvailableAreaType.setAreaType(a.getAreaType());
            muAvailableAreaType.setMoAvailableAreaType(a);
            muAvailableAreaType.setCreateDate(LocalDateTime.now());
            muAvailableAreaTypesRepository.save(muAvailableAreaType);
        });

    }

    @Override
    public void addMuAvailableAreaTypesV3(long moId, long muId, List<Long> areaTypeCodes) throws ContingentException {
        areaTypeCodes = areaTypeCodes.stream().distinct().collect(Collectors.toList());
        Validation validation = new Validation();
        List<AreaType> areaTypes = areaHelper.checkAndGetAreaTypesExist(areaTypeCodes, validation);

        areaHelper.checkAreaTypesExistInMU(muId, areaTypes, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        areaTypes.forEach(areaType -> {
            MuAvailableAreaTypes muAvailableAreaType = new MuAvailableAreaTypes();
            muAvailableAreaType.setMuId(muId);
            muAvailableAreaType.setAreaType(areaType);
            muAvailableAreaType.setCreateDate(LocalDateTime.now());

            muAvailableAreaTypesRepository.save(muAvailableAreaType);

        });
    }


    @Override
    public void delMuAvailableAreaTypes(long muId, List<Long> areaTypeCodes) throws ContingentException {
        areaTypeCodes = areaTypeCodes.stream().distinct().collect(Collectors.toList());
        Validation validation = new Validation();
        areaTypeCodes = areaHelper.checkAndGetAreaTypesExist(areaTypeCodes, validation).stream()
                .map(AreaType::getCode)
                .collect(Collectors.toList());
        // 1.
        List<MuAvailableAreaTypes> areaTypes = areaHelper.checkAndGetAreaTypesNotExistInMU(muId, areaTypeCodes, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        // 2.
        muAvailableAreaTypesRepository.deleteAll(areaTypes);

    }

    @Override
    public MuAreaTypesFull getMuAvailableAreaTypes(long moId, long muId, AreaTypeStateType areaTypeState) throws ContingentException {
        // 1.
        List<AreaType> usedAreaTypes = muAvailableAreaTypesRepository.findAreaTypes(muId).stream()
                .map(MuAvailableAreaTypes::getAreaType)
                .collect(Collectors.toList());
        // 2.
        List<AreaType> availableAreaTypes = new ArrayList<>();

        if (!AreaTypeStateType.USED_IN_MU.equals(areaTypeState)) {
            availableAreaTypes.addAll(moAvailableAreaTypesRepository.findAreaTypes(moId).stream()
                    .filter(a -> usedAreaTypes.stream()
                            .noneMatch(b -> Objects.equals(b, a.getAreaType())))
                    .map(MoAvailableAreaTypes::getAreaType)
                    .collect(Collectors.toList()));
        }
        return new MuAreaTypesFull(AreaTypeStateType.AVAILABLE_TO_ADD.equals(areaTypeState) ? new ArrayList<>() : usedAreaTypes,
                availableAreaTypes);
    }

    @Override
    public void delMoAddress(List<Long> moAddressIds, long orderId) throws ContingentException {
        Validation validation = new Validation();

        // 1.
        if (moAddressIds.size() > settingService.getPar2()) {
            validation.error(AreaErrorReason.TOO_MANY_ADDRESSES, new ValidationParameter("maxAddresses", settingService.getPar2()));
        }

        moAddressIds = moAddressIds.stream().distinct().collect(Collectors.toList());
        // 2.
        List<MoAddress> moAddresses = areaHelper.getAndCheckMoAddressesExist(moAddressIds, validation);

        // 3.
        areaHelper.checkOrderExists(orderId, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        //6. - 9.
        deleteMuAndAreaAddresses(moAddresses, false);
    }

    @Override
    public boolean delMoAddressTotal(long orderId, List<Long> addressGlobalIds) throws ContingentException {
        Validation validation = new Validation();
        //2.
        areaHelper.checkMaxRequestAddresses(addressGlobalIds.size(), validation);
        //3.
        areaHelper.checkOrderExists(orderId, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        //4.
        List<MoAddress> moAddresses = moAddressRepository.getActiveMoAddressesByGlobalIds(addressGlobalIds.stream()
                .distinct()
                .collect(Collectors.toList())).getContent();
        //5. Система закрывает территории обслуживания
        deleteMuAndAreaAddresses(moAddresses, true);

        return true;
    }

    @Override
    public Page<MoAddress> getMoAddress(long moId, List<Long> areaTypeCodes, PageRequest paging) throws ContingentException {
        Validation validation = new Validation();

        // 1.
        areaHelper.checkMaxPage(paging, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        // 2.
        areaHelper.checkAndGetAreaTypesExist(areaTypeCodes, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        return moAddressRepository.getActiveMoAddresses(moId, areaTypeCodes, paging);
    }

    @Override
    public Page<MoAddressWithAddresses> searchMoAddress(long moId, List<Long> addressGlobalIds, List<Long> areaTypeCodes, LocalDate orderDate,
                                                        String orderName, String orderNumber, String orderOuz, LocalDate orderCreateDate,
                                                        PageRequest paging) throws ContingentException {
        Validation validation = new Validation();
        //2.
        areaHelper.checkMaxPage(paging, validation);
        //3.
        List<AreaType> areaTypes = areaHelper.checkAndGetAreaTypesExist(areaTypeCodes, validation);
        areaHelper.checkAreaTypesArePrimary(areaTypes, validation);
        areaHelper.checkAreaTypesServeTerritory(areaTypes, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        //4.
        Page<MoAddress> addresses = moAddressRepository.find(moId, addressGlobalIds, areaTypeCodes, orderDate, orderName,
                orderNumber, orderOuz, orderCreateDate, paging);
        Map<MoAddress, List<AreaAddress>> areaAddressesMap = new HashMap<>();

        if (!addresses.isEmpty()) {
            //5.
            List<AreaAddress> areaAddresses = areaAddressRepository.findActualAreaAddresses(addresses.stream()
                    .map(MoAddress::getId)
                    .distinct()
                    .collect(Collectors.toList()));
            areaAddressesMap.putAll(areaAddresses.stream()
                    .collect(Collectors.groupingBy(AreaAddress::getMoAddress)));
        }
        List<MoAddressWithAddresses> mappedAddresses = addresses.stream()
                .map(m -> new MoAddressWithAddresses(m, !areaAddressesMap.containsKey(m) ? Collections.emptyList() :
                        areaAddressesMap.get(m).stream()
                                .filter(a -> Objects.equals(a.getAddress(), m.getAddress()))
                                .collect(Collectors.toList())))
                .collect(Collectors.toList());

        return paging == null ? new PageImpl<>(mappedAddresses) : new PageImpl<>(mappedAddresses, paging, addresses.getTotalElements());
    }

    @Override
    public List<AreaType> getMuAvailableAreaTypes2(long muId) {
        return muAvailableAreaTypesRepository.findAreaTypes(muId).stream()
                .map(MuAvailableAreaTypes::getAreaType)
                .collect(Collectors.toList());
    }

    /**
     * Часть алгоритмов delMoAddress и delMoAddressTotal
     * @param moAddresses
     */
    private void deleteMuAndAreaAddresses(List<MoAddress> moAddresses, boolean total) {
        if (moAddresses.isEmpty()) {
            return;
        }
        List<Long> moAddressesIds = moAddresses.stream()
                .map(MoAddress::getId)
                .collect(Collectors.toList());
        List<AreaAddress> areaAddresses = areaAddressRepository.findAreaAddresses(moAddressesIds);

        if (!areaAddresses.isEmpty()) {
            if (total) {
                areaHelper.deleteAreaAddressTotal(areaAddresses);
            } else {
                areaHelper.deleteAreaAddress(areaAddresses);
            }
        }
        areaHelper.delMoAddresses(moAddresses);

        for (MoAddress moAddress: moAddresses) {
            historyHelper.sendHistory(moAddress, null, MoAddress.class);
        }
    }

    @Override
    public List<MuMuService> getMuMuService(long muId, long areaTypeCode) throws ContingentException {
        Validation validation = new Validation();
        List<AreaType> areaTypes = areaHelper.checkAndGetAreaTypesExist(Arrays.asList(areaTypeCode), validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        List<MuMuService> results = muMuServiceRepository.getMuMuServices(muId, areaTypes.get(0));

        if (results.isEmpty()) {
            validation.error(AreaErrorReason.MU_SERVICE_NOT_FOUND);
            throw new ContingentException(validation);
        }
        return results;
    }
}
