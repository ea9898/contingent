package moscow.ptnl.contingent.area.service;

import moscow.ptnl.contingent.area.entity.area.AddressAllocationOrder;
import moscow.ptnl.contingent.area.entity.area.Area;
import moscow.ptnl.contingent.area.entity.area.AreaMedicalEmployee;
import moscow.ptnl.contingent.area.entity.area.AreaToAreaType;
import moscow.ptnl.contingent.area.entity.area.MuProfile;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeEnum;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeMedicalPositions;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypes;
import moscow.ptnl.contingent.area.entity.nsi.Specialization;
import moscow.ptnl.contingent.area.error.AreaErrorReason;
import moscow.ptnl.contingent.area.error.ContingentException;
import moscow.ptnl.contingent.area.error.Validation;
import moscow.ptnl.contingent.area.error.ValidationParameter;
import moscow.ptnl.contingent.area.model.esu.AreaCreateEvent;
import moscow.ptnl.contingent.area.model.esu.AreaUpdateEvent;
import moscow.ptnl.contingent.area.repository.area.AddressAllocationOrderCRUDRepository;
import moscow.ptnl.contingent.area.repository.area.AddressAllocationOrderRepository;
import moscow.ptnl.contingent.area.repository.area.AreaCRUDRepository;
import moscow.ptnl.contingent.area.repository.area.AreaMedicalEmployeeCRUDRepository;
import moscow.ptnl.contingent.area.repository.area.AreaRepository;
import moscow.ptnl.contingent.area.repository.area.AreaToAreaTypeCRUDRepository;
import moscow.ptnl.contingent.area.repository.area.AreaToAreaTypeRepository;
import moscow.ptnl.contingent.area.repository.area.MuProfileCRUDRepository;
import moscow.ptnl.contingent.area.repository.area.MuProfileRepository;
import moscow.ptnl.contingent.area.repository.nsi.AreaTypeMedicalPositionsRepository;
import moscow.ptnl.contingent.area.repository.nsi.AreaTypesCRUDRepository;
import moscow.ptnl.contingent.area.repository.nsi.MuProfileTemplatesRepository;
import moscow.ptnl.contingent.area.repository.nsi.SpecializationToPositionNomRepository;
import moscow.ptnl.util.Strings;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Component;
import ru.mos.emias.contingent2.core.AddMedicalEmployee;
import ru.mos.emias.contingent2.core.ChangeMedicalEmployee;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

@Component
public class AreaServiceInternalImpl implements AreaServiceInternal {

    @Autowired
    private MuProfileRepository muProfileRepository;

    @Autowired
    private MuProfileCRUDRepository muProfileCRUDRepository;

    @Autowired
    private MuProfileTemplatesRepository muProfileTemplatesRepository;

    @Autowired
    private AreaTypesCRUDRepository areaTypesCRUDRepository;

    @Autowired
    private AreaRepository areaRepository;

    @Autowired
    private AreaCRUDRepository areaCRUDRepository;

    @Autowired
    private AreaToAreaTypeCRUDRepository areaToAreaTypeCRUDRepository;

    @Autowired
    private AreaToAreaTypeRepository areaToAreaTypeRepository;

    @Autowired
    private AddressAllocationOrderCRUDRepository addressAllocationOrderCRUDRepository;

    @Autowired
    private AddressAllocationOrderRepository addressAllocationOrderRepository;

    @Autowired
    private AreaMedicalEmployeeCRUDRepository areaMedicalEmployeeCRUDRepository;

    @Autowired
    private SpecializationToPositionNomRepository specializationToPositionNomRepository;

    @Autowired
    private AreaTypeMedicalPositionsRepository areaTypeMedicalPositionsRepository;

    @Autowired
    private AreaChecker areaChecker;

    @Autowired
    private EsuService esuService;

    @Override
    public List<MuProfile> getProfileMU(Long muId) throws ContingentException {
        return muProfileRepository.getMuProfilesByMuId(muId);
    }

    /**
     *
     * @param muId
     * @param muTypeId
     * @param areaTypeCodes
     * @return
     * @throws ContingentException
     */
    @Override
    public void addProfileMU(Long muId, Long muTypeId, List<Long> areaTypeCodes) throws ContingentException {
        Validation validation = new Validation();

        areaChecker.checkAreaTypesExist(areaTypeCodes, validation, "areaTypesAdd");

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        areaChecker.checkProfileExist(muId, areaTypeCodes, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        areaChecker.checkMuProfileCreateAvailableByMuType(muTypeId, areaTypeCodes, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        for (Long areaTypeCode: areaTypeCodes) {
            AreaTypes areaType = areaTypesCRUDRepository.findById(areaTypeCode).get();
            MuProfile muProfile = new MuProfile(muId, areaType);
            muProfileCRUDRepository.save(muProfile);
        }

        return;
    }

    @Override
    public void delProfileMU(Long muId, Long muTypeId, List<Long> areaTypeCodes) throws ContingentException {
        Validation validation = new Validation();

        areaChecker.checkMuProfilesHasAreaTypes(muId, areaTypeCodes, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        areaChecker.checkMuActiveAreasNotExist(muId, areaTypeCodes, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        areaChecker.checkMuProfileCreateAvailableByMuType(muTypeId, areaTypeCodes, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        List<MuProfile> muProfiles = muProfileRepository.getMuProfilesByMuId(muId);
        List<MuProfile> profilesToDelete = muProfiles.stream()
                .filter(m -> m.getAreaType() != null
                        && areaTypeCodes.contains(m.getAreaType().getCode()))
                .collect(Collectors.toList());

        if (!profilesToDelete.isEmpty()) {
            muProfileCRUDRepository.deleteAll(profilesToDelete);
        }

    }

    @Override
    public Long createPrimaryArea(long moId, long muId, Integer number, Long areaTypeCode,
                             Integer ageMin, Integer ageMax, Integer ageMinM, Integer ageMaxM, Integer ageMinW, Integer ageMaxW,
                             boolean autoAssignForAttachment, Boolean attachByMedicalReason, String description) throws ContingentException {
        Validation validation = new Validation();

        areaChecker.checkAreaTypesExist(Collections.singletonList(areaTypeCode), validation, "areaTypeCode");

        MuProfile muProfile = muProfileRepository.getMuProfilesByMuId(muId).stream()
                .filter(p -> p.getAreaType() != null && Objects.equals(p.getAreaType().getCode(), areaTypeCode))
                .findFirst().orElse(null);

        if (muProfile == null) {
            validation.error(AreaErrorReason.MU_PROFILE_HAS_NO_AREA_TYPE, new ValidationParameter("areaTypeCode", areaTypeCode));
        }
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        Boolean mpguAvailable = muProfile.getAreaType().getAttributes() == null ? null : muProfile.getAreaType().getAttributes().getMpguAvailable();
        Boolean areaTypeAttachByMedicalReason = muProfile.getAreaType().getAttributes() == null ? null : muProfile.getAreaType().getAttributes().getAttachByMedicalReason();
        //Todo сделать проерку AREA_COUNT_LIMIT после разработки НСИ
        if (muProfile.getAreaType().getKindAreaType() != null &&
                //Todo уточнить код вида участка «Мягко - ассоциированный» и вынести в настройки
                Objects.equals(muProfile.getAreaType().getKindAreaType().getCode(), 1L)) {
            if (Strings.isNullOrEmpty(description) || number == null ||
                    (ageMin == null && ageMax == null && ageMinM == null && ageMaxM == null && ageMinW == null && ageMaxW == null)) {
                validation.error(AreaErrorReason.SOFT_RELATED_AREA_MUST_BE_FILLED);
            }
        }
        List<Area> areas = areaRepository.findAreas(null, muId, areaTypeCode, number, true);

        if (!areas.isEmpty()) {
            validation.error(AreaErrorReason.AREA_WITH_TYPE_AND_NUMBER_EXISTS_IN_MO, new ValidationParameter("areaTypeCode", areaTypeCode),
                    new ValidationParameter("number", number));
        }
        areaChecker.checkAreaTypeAgeSetups(muProfile.getAreaType(), ageMin, ageMax, ageMinM, ageMaxM, ageMinW, ageMaxW, validation);

        if (autoAssignForAttachment) {
            if (!Boolean.TRUE.equals(mpguAvailable)) {
                validation.error(AreaErrorReason.CANT_SET_AUTO_ASSIGN_FOR_ATTACHMENT, new ValidationParameter("areaTypeCode", areaTypeCode));
            }
            if (Boolean.TRUE.equals(attachByMedicalReason)) {
                validation.error(AreaErrorReason.AREA_FLAGS_INCORRECT);
            }
        }
        if (attachByMedicalReason != null && areaTypeAttachByMedicalReason != null &&
                !Objects.equals(attachByMedicalReason, areaTypeAttachByMedicalReason)) {
            validation.error(AreaErrorReason.ATTACH_BY_MEDICAL_REASON_INCORRECT,
                    new ValidationParameter("attachByMedicalReason", attachByMedicalReason),
                    new ValidationParameter("attachByMedicalReason", areaTypeAttachByMedicalReason));
        }
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        //Создание новго первичного участка
        Area area = new Area(moId, muId, muProfile.getAreaType(), number, autoAssignForAttachment, false, description,
                attachByMedicalReason, ageMin, ageMax, ageMinM, ageMaxM, ageMinW, ageMaxW, LocalDateTime.now());
        areaCRUDRepository.save(area);

        resetAutoAssignForAttachment(area);
        esuService.saveAndPublishToESU(new AreaCreateEvent(area, null));

        return area.getId();
    }

    @Override
    public Long createDependentArea(long moId, long muId, Integer number, Long areaTypeCode, List<Long> primaryAreaTypeCodes,
                             Integer ageMin, Integer ageMax, Integer ageMinM, Integer ageMaxM, Integer ageMinW, Integer ageMaxW,
                             boolean autoAssignForAttachment, String description) throws ContingentException {
        Validation validation = new Validation();

        areaChecker.checkAreaTypesExist(Collections.singletonList(areaTypeCode), validation, "areaTypeCode");
        Map<Long, AreaTypes> primaryAreaTypes = areaChecker.checkAndGetPrimaryAreaTypesInMU(muId, primaryAreaTypeCodes, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        areaChecker.checkPrimaryAreasInMU(muId, primaryAreaTypeCodes, validation);

        if (!areaRepository.findAreas(moId, null, areaTypeCode, null, null).isEmpty()) {
            validation.error(AreaErrorReason.AREA_WITH_TYPE_EXISTS_IN_MO, new ValidationParameter("areaTypeCode", areaTypeCode));
        }
        if (number != null &&
                !areaRepository.findAreas(null, muId, areaTypeCode, number, true).isEmpty()) {
            validation.error(AreaErrorReason.AREA_WITH_TYPE_AND_NUMBER_EXISTS_IN_MO, new ValidationParameter("areaTypeCode", areaTypeCode),
                    new ValidationParameter("number", number));
        }
        AreaTypes areaType = areaTypesCRUDRepository.findById(areaTypeCode).get();
        areaChecker.checkAreaTypeAgeSetups(areaType, ageMin, ageMax, ageMinM, ageMaxM, ageMinW, ageMaxW, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        //Создание новго зависимого участка
        Area area = new Area(moId, muId, areaType, number, autoAssignForAttachment, false, description,
                null, ageMin, ageMax, ageMinM, ageMaxM, ageMinW, ageMaxW, LocalDateTime.now());
        areaCRUDRepository.save(area);
        //Сохранение привязки к первичным типам участка
        primaryAreaTypeCodes.forEach(c -> {
            AreaToAreaType areaToAreaType = new AreaToAreaType();
            areaToAreaType.setArea(area);
            areaToAreaType.setAreaType(primaryAreaTypes.get(c));
            areaToAreaTypeCRUDRepository.save(areaToAreaType);
        });
        esuService.saveAndPublishToESU(new AreaCreateEvent(area, areaToAreaTypeRepository.getAreaTypesByAreaId(area.getId())));

        return area.getId();
    }

    @Override
    public void updatePrimaryArea(long areaId, Integer number,
                           Integer ageMin, Integer ageMax, Integer ageMinM, Integer ageMaxM, Integer ageMinW, Integer ageMaxW,
                           boolean autoAssignForAttachment, Boolean attachByMedicalReason, String description) throws ContingentException {
        Validation validation = new Validation();
        Area area = areaChecker.checkAndGetArea(areaId, validation);
        Area oldArea = new Area(area);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        Boolean mpguAvailable = area.getAreaType().getAttributes() == null ? null : area.getAreaType().getAttributes().getMpguAvailable();
        Boolean areaTypeAttachByMedicalReason = area.getAreaType().getAttributes() == null ? null : area.getAreaType().getAttributes().getAttachByMedicalReason();

        if (number != null && !number.equals(area.getNumber())) {
            List<Area> areas = areaRepository.findAreas(null, area.getMuId(), area.getAreaType().getCode(), number, true);

            if (!areas.isEmpty()) {
                validation.error(AreaErrorReason.AREA_WITH_TYPE_AND_NUMBER_EXISTS_IN_MO,
                        new ValidationParameter("areaTypeCode", area.getAreaType().getCode()),
                        new ValidationParameter("number", number));
            }
        }
        if (autoAssignForAttachment) {
            if (!Boolean.TRUE.equals(mpguAvailable)) {
                validation.error(AreaErrorReason.CANT_SET_AUTO_ASSIGN_FOR_ATTACHMENT,
                        new ValidationParameter("areaTypeCode", area.getAreaType().getCode()));
            }
            if (Boolean.TRUE.equals(attachByMedicalReason)) {
                validation.error(AreaErrorReason.AREA_FLAGS_INCORRECT);
            }
        }
        if (attachByMedicalReason != null && areaTypeAttachByMedicalReason != null &&
                !Objects.equals(attachByMedicalReason, areaTypeAttachByMedicalReason)) {
            validation.error(AreaErrorReason.ATTACH_BY_MEDICAL_REASON_INCORRECT,
                    new ValidationParameter("attachByMedicalReason", attachByMedicalReason),
                    new ValidationParameter("attachByMedicalReason", areaTypeAttachByMedicalReason));
        }
        areaChecker.checkAreaTypeAgeSetups(area.getAreaType(), ageMin, ageMax, ageMinM, ageMaxM, ageMinW, ageMaxW, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        area.setNumber(number == null ? area.getNumber() : number);
        area.setAgeMax(ageMax == null ? area.getAgeMax() : ageMax);
        area.setAgeMin(ageMin == null ? area.getAgeMin() : ageMin);
        area.setAgeMMax(ageMaxM == null ? area.getAgeMMax() : ageMaxM);
        area.setAgeMMin(ageMinM == null ? area.getAgeMMin() : ageMinM);
        area.setAgeWMax(ageMaxW == null ? area.getAgeWMax() : ageMaxW);
        area.setAgeWMin(ageMinW == null ? area.getAgeWMin() : ageMinW);
        area.setAutoAssignForAttach(autoAssignForAttachment);
        area.setAttachByMedicalReason(attachByMedicalReason == null ? area.getAttachByMedicalReason() : attachByMedicalReason);
        area.setDescription(description == null ? area.getDescription() : description);
        area.setUpdateDate(LocalDateTime.now());

        resetAutoAssignForAttachment(area);

        esuService.saveAndPublishToESU(new AreaUpdateEvent(area, oldArea, null, null));
    }

    @Override
    public void updateDependentArea(long areaId, Long muId, Integer number, List<Long> primaryAreaTypeCodesAdd,
                                    List<Long> primaryAreaTypeCodesDel,
                                    Integer ageMin, Integer ageMax, Integer ageMinM, Integer ageMaxM, Integer ageMinW, Integer ageMaxW,
                                    boolean autoAssignForAttachment, String description) throws ContingentException {
        Validation validation = new Validation();
        Area area = areaChecker.checkAndGetArea(areaId, validation);
        Area oldArea = new Area(area);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        Long muIdFinal = muId == null ? area.getMuId() : muId;

        if (number != null && !number.equals(area.getNumber())) {
            List<Area> areas = areaRepository.findAreas(null, muIdFinal, area.getAreaType().getCode(), number, true);

            if (!areas.isEmpty()) {
                validation.error(AreaErrorReason.AREA_WITH_TYPE_AND_NUMBER_EXISTS_IN_MO,
                        new ValidationParameter("areaTypeCode", area.getAreaType().getCode()),
                        new ValidationParameter("number", number));
            }
        }
        if (muId != null && !muId.equals(area.getMuId())) {
            if (area.getAreaType().getKindAreaType() != null &&
                    //Todo уточнить код вида участка «Ассоциированный со специализированным кабинетом» и вынести в настройки
                    !Objects.equals(area.getAreaType().getKindAreaType().getCode(), 2L)) {
                validation.error(AreaErrorReason.AREA_NOT_RELATED_TO_SPECIAL_OFFICE);
            }
        }
        Map<Long, AreaTypes> primaryAreaTypes = areaChecker.checkAndGetPrimaryAreaTypesInMU(muIdFinal, primaryAreaTypeCodesAdd, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        areaChecker.checkPrimaryAreasInMU(muIdFinal, primaryAreaTypeCodesAdd, validation);
        areaChecker.checkAreaTypeAgeSetups(area.getAreaType(), ageMin, ageMax, ageMinM, ageMaxM, ageMinW, ageMaxW, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        //Обновление участка
        area.setMuId(muIdFinal);
        area.setNumber(number == null ? area.getNumber() : number);
        area.setAgeMax(ageMax == null ? area.getAgeMax() : ageMax);
        area.setAgeMin(ageMin == null ? area.getAgeMin() : ageMin);
        area.setAgeMMax(ageMaxM == null ? area.getAgeMMax() : ageMaxM);
        area.setAgeMMin(ageMinM == null ? area.getAgeMMin() : ageMinM);
        area.setAgeWMax(ageMaxW == null ? area.getAgeWMax() : ageMaxW);
        area.setAgeWMin(ageMinW == null ? area.getAgeWMin() : ageMinW);
        area.setAutoAssignForAttach(autoAssignForAttachment);
        area.setDescription(description == null ? area.getDescription() : description);
        area.setUpdateDate(LocalDateTime.now());

        resetAutoAssignForAttachment(area);
        //Обновление привязки к первичным типам участка
        List<AreaToAreaType> areaToAreaTypes = areaToAreaTypeRepository.getAreaTypesByAreaId(area.getId());

        List<AreaToAreaType> areaToAreaTypesToAdd = new ArrayList<>();

        primaryAreaTypeCodesAdd.stream()
                .filter(c -> areaToAreaTypes.stream().noneMatch(a -> Objects.equals(c, a.getAreaType().getCode())))
                .forEach(c -> {
            AreaToAreaType areaToAreaType = new AreaToAreaType();
            areaToAreaType.setArea(area);
            areaToAreaType.setAreaType(primaryAreaTypes.get(c));
            areaToAreaTypeCRUDRepository.save(areaToAreaType);
            areaToAreaTypesToAdd.add(areaToAreaType);
        });
        List<AreaToAreaType> areaToAreaTypesToRemove = areaToAreaTypes.stream()
                .filter(a -> a.getAreaType() != null &&
                        !primaryAreaTypeCodesAdd.contains(a.getAreaType().getCode()) &&
                        primaryAreaTypeCodesDel.contains(a.getAreaType().getCode()))
                .collect(Collectors.toList());

        areaToAreaTypeCRUDRepository.deleteAll(areaToAreaTypesToRemove);

        esuService.saveAndPublishToESU(new AreaUpdateEvent(area, oldArea, areaToAreaTypesToAdd, areaToAreaTypesToRemove));
    }

    private void resetAutoAssignForAttachment(Area area) {
        if (area.getAutoAssignForAttach()) {
            List<Area> areas = areaRepository.findAreas(null, area.getMuId(), area.getAreaType().getCode(), null, null);
            areas.stream().filter(a -> !Objects.equals(area.getId(), a.getId())).forEach(a -> a.setAutoAssignForAttach(false));
        }
    }

    @Override
    public Long createOrder(String number, LocalDate date, String ouz, String name) throws ContingentException {
        Validation validation = new Validation();
        areaChecker.checkDateTillToday(date, validation);

        if (!addressAllocationOrderRepository.findAddressAllocationOrders(number, date, ouz, name, false).isEmpty()) {
            validation.error(AreaErrorReason.ADDRESS_ALLOCATION_ORDER_EXISTS);
        }
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        AddressAllocationOrder order = new AddressAllocationOrder();
        order.setCreateDate(LocalDateTime.now());
        order.setUpdateDate(LocalDateTime.now());
        order.setArchived(false);
        order.setNumber(number);
        order.setDate(date);
        order.setOuz(ouz);
        order.setName(name);

        return addressAllocationOrderCRUDRepository.save(order).getId();
    }

    @Override
    public void updateOrder(Long id, String number, LocalDate date, String ouz, String name) throws ContingentException {
        Validation validation = new Validation();
        AddressAllocationOrder order = addressAllocationOrderCRUDRepository.findById(id).orElse(null);

        if (order == null) {
            validation.error(AreaErrorReason.ADDRESS_ALLOCATION_ORDER_NOT_EXISTS,
                    new ValidationParameter("id", id));
            throw new ContingentException(validation);
        }
        if (Boolean.TRUE.equals(order.getArchived())) {
            validation.error(AreaErrorReason.ADDRESS_ALLOCATION_ORDER_IS_ARCHIVED,
                    new ValidationParameter("id", id));
        }
        if (date != null) {
            areaChecker.checkDateTillToday(date, validation);
        }
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
        order.setUpdateDate(LocalDateTime.now());
        order.setNumber(numberNew);
        order.setDate(dateNew);
        order.setOuz(ouzNew);
        order.setName(nameNew);
    }

    @Override
    public Page<AddressAllocationOrder> searchOrder(Long id, String number, LocalDate date, String name, PageRequest paging) throws ContingentException {
        Validation validation = new Validation();

        if (id == null && number == null && date == null && name == null) {
            validation.error(AreaErrorReason.NO_SEARCH_PARAMETERS);
            throw new ContingentException(validation);
        }
        return addressAllocationOrderRepository.findAddressAllocationOrdersOverlapped(id, number, date, name, paging);
    }

    @Override
    public Area getAreaById(Long id) throws ContingentException {
        Optional<Area> area = areaCRUDRepository.findById(id);

        return area.orElseThrow(() -> new ContingentException(
                new Validation().error(AreaErrorReason.AREA_NOT_FOUND, new ValidationParameter("areaId", id))));
    }

    @Override
    public List<Long> setMedicalEmployeeOnArea(long areaId, List<AddMedicalEmployee> addMedicalEmployees,
                                               List<ChangeMedicalEmployee> changeMedicalEmployees,
                                               List<Long> deleteMedicalEmployees) throws ContingentException {

        //1 Система выполняет поиск в БД (AREAS) участка с переданным ИД. Участок найден, иначе возвращает ошибку
        Area area = areaCRUDRepository.findById(areaId).orElseThrow(() -> new ContingentException(
                new Validation().error(AreaErrorReason.AREA_NOT_FOUND, new ValidationParameter("areaId", areaId))));

        //2 Система проверяет, что участок не находится в архиве (AREAS.ARCHIVE = 0), иначе возвращает ошибку
        if (area.getArchived()) {
            throw new ContingentException(new Validation().error(
                    AreaErrorReason.AREA_IS_ARCHIVED, new ValidationParameter("areaId", areaId)));
        }


        List<String> changeIds = changeMedicalEmployees.stream().map(empl -> String.valueOf(empl.getAssignmentId())).collect(Collectors.toList());
        Iterable<AreaMedicalEmployee> changeEmployees = areaMedicalEmployeeCRUDRepository.findAllById(changeIds);

        //3
        if (area.getAreaType().getCode() != AreaTypeEnum.MILDLY_ASSOCIATED.getCode()
                && area.getAreaType().getCode() != AreaTypeEnum.TREATMENT_ROOM_ASSOCIATED.getCode()) {
            if (addMedicalEmployees.stream().anyMatch(AddMedicalEmployee::isIsReplacement)) {
                throwException(AreaErrorReason.AREA_NOT_RELATED_TO_MILDLY_ASSOCIATED);
            }
            List<String> deleteIds = deleteMedicalEmployees.stream().map(String::valueOf).collect(Collectors.toList());
            Iterable<AreaMedicalEmployee> deleteEmployees = areaMedicalEmployeeCRUDRepository.findAllById(deleteIds);
            for (AreaMedicalEmployee empl : deleteEmployees) {
                if (!empl.getReplacement()) {
                    throwException(AreaErrorReason.AREA_NOT_RELATED_TO_MILDLY_ASSOCIATED);
                }
            }

            for (AreaMedicalEmployee empl : changeEmployees) {
                if (!empl.getReplacement()) {
                    throwException(AreaErrorReason.AREA_NOT_RELATED_TO_MILDLY_ASSOCIATED);
                }
            }
        }

        //4
        if (!changeMedicalEmployees.isEmpty()) {
            for (AreaMedicalEmployee empl : changeEmployees) {

                //4.1
                if (empl.getEndDate()!= null && empl.getEndDate().isBefore(LocalDate.now()) || empl.getArea().getId() != areaId || empl.isDeleted()) {
                    throwException(AreaErrorReason.EMPLOYEE_NOT_RELATED_TO_AREA,
                            new ValidationParameter("id",empl.getId()));
                }

                //4.2
                ChangeMedicalEmployee inputEmpl = changeMedicalEmployees.stream().filter(
                        em -> empl.getId().equals(String.valueOf(em.getAssignmentId()))).findFirst().get();
                if (inputEmpl.getEndDate() == null && inputEmpl.getStartDate() == null) {
                    throwException(AreaErrorReason.NOTHING_TO_CHANGE);
                }

                //4.3
                LocalDate startDate = inputEmpl.getStartDate() != null ? inputEmpl.getStartDate() : empl.getStartDate();
                LocalDate endDate = inputEmpl.getEndDate() != null ? inputEmpl.getEndDate() : empl.getEndDate();
                if (startDate != null && endDate != null && startDate.isAfter(endDate)) {
                    throwException(AreaErrorReason.START_DATE_IS_AFTER_END_DATE,
                            new ValidationParameter("endDate",endDate),
                            new ValidationParameter("startDate", startDate));
                }
            }
        }

        //5
        if (!addMedicalEmployees.isEmpty()) {
            for (AddMedicalEmployee empl : addMedicalEmployees) {
                //5.1
                if (empl.getStartDate().isBefore(LocalDate.now())) {
                    throwException(AreaErrorReason.START_DATE_IN_PAST, new ValidationParameter("startDate", empl.getStartDate()));
                }

                //5.2
                Specialization specialization = specializationToPositionNomRepository.getSpecializationIdByPositionNomId(empl.getPositionId());

                //5.3
                if (specialization!= null && area.getAreaType().getSpecialization().getId().equals(specialization.getId())) {
                    throwException(AreaErrorReason.SPECIALIZATION_NOT_RELATED_TO_AREA,
                            new ValidationParameter("InputSpecialization",specialization.getTitle()),
                            new ValidationParameter("id", empl.getMedicalEmployeeJobInfoId()),
                            new ValidationParameter("AreaSpecialization", area.getAreaType().getSpecialization().getTitle()));
                }

                //5.4
                List<AreaTypeMedicalPositions> positions = areaTypeMedicalPositionsRepository.getPositionsByAreaType(area.getAreaType().getCode());
                if (positions != null && positions.stream().noneMatch(pos -> pos.getMedicalPositionId() == empl.getPositionId())) {
                    throwException(AreaErrorReason.POSITION_NOT_SET_FOR_AREA_TYPE,
                            new ValidationParameter("specialization", specialization.getTitle()),
                            new ValidationParameter("id", empl.getMedicalEmployeeJobInfoId()),
                            new ValidationParameter("areaType", area.getAreaType().getName()));
                }
            }
        }

        //6
        return null;
    }

    private void throwException(AreaErrorReason e, ValidationParameter... params) throws ContingentException {
        throw new ContingentException(new Validation().error(e,params));
    }
}
