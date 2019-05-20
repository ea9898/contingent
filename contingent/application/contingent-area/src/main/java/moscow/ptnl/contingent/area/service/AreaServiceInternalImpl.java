package moscow.ptnl.contingent.area.service;

import moscow.ptnl.contingent.area.entity.area.AddressAllocationOrder;
import moscow.ptnl.contingent.area.entity.area.Area;
import moscow.ptnl.contingent.area.entity.area.AreaMedicalEmployee;
import moscow.ptnl.contingent.area.entity.area.AreaToAreaType;
import moscow.ptnl.contingent.area.entity.area.MuProfile;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeMedicalPositions;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypes;
import moscow.ptnl.contingent.area.entity.nsi.KindAreaTypeEnum;
import moscow.ptnl.contingent.area.entity.nsi.PositionNomClinic;
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
import moscow.ptnl.contingent.area.repository.area.AreaMedicalEmployeeRepository;
import moscow.ptnl.contingent.area.repository.area.AreaRepository;
import moscow.ptnl.contingent.area.repository.area.AreaToAreaTypeCRUDRepository;
import moscow.ptnl.contingent.area.repository.area.AreaToAreaTypeRepository;
import moscow.ptnl.contingent.area.repository.area.MuProfileCRUDRepository;
import moscow.ptnl.contingent.area.repository.area.MuProfileRepository;
import moscow.ptnl.contingent.area.repository.nsi.AreaTypeMedicalPositionsRepository;
import moscow.ptnl.contingent.area.repository.nsi.AreaTypesCRUDRepository;
import moscow.ptnl.contingent.area.repository.nsi.MuProfileTemplatesRepository;
import moscow.ptnl.contingent.area.repository.nsi.PositionNomClinicCRUDRepository;
import moscow.ptnl.contingent.area.repository.nsi.SpecializationToPositionNomRepository;
import moscow.ptnl.contingent.area.util.Period;
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
import java.util.Comparator;
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
    private AreaMedicalEmployeeRepository areaMedicalEmployeeRepository;

    @Autowired
    private PositionNomClinicCRUDRepository positionNomClinicCRUDRepository;

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
                Objects.equals(muProfile.getAreaType().getKindAreaType().getCode(), KindAreaTypeEnum.MILDLY_ASSOCIATED.getCode())) {
            if (Strings.isNullOrEmpty(description) || number == null ||
                    (ageMin == null && ageMax == null && ageMinM == null && ageMaxM == null && ageMinW == null && ageMaxW == null)) {
                validation.error(AreaErrorReason.SOFT_RELATED_AREA_MUST_BE_FILLED);
            }
        }
        areaChecker.checkAreaExistsInMU(muId, areaTypeCode, number, null, validation);
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
        if (number != null) {
            areaChecker.checkAreaExistsInMU(muId, areaTypeCode, number, null, validation);
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
//            areaToAreaType.setAreaType(primaryAreaTypes.get(c));
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

        if (number != null) {
            areaChecker.checkAreaExistsInMU(area.getMuId(), area.getAreaType().getCode(), number, area.getId(), validation);
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

        if (number != null) {
            areaChecker.checkAreaExistsInMU(muIdFinal, area.getAreaType().getCode(), number, area.getId(), validation);
        }
        if (muId != null && !muId.equals(area.getMuId())) {
            if (area.getAreaType().getKindAreaType() != null &&
                    !Objects.equals(area.getAreaType().getKindAreaType().getCode(), KindAreaTypeEnum.TREATMENT_ROOM_ASSOCIATED.getCode())) {
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

//        primaryAreaTypeCodesAdd.stream()
//                .filter(c -> areaToAreaTypes.stream().noneMatch(a -> Objects.equals(c, a.getAreaType().getCode())))
//                .forEach(c -> {
//            AreaToAreaType areaToAreaType = new AreaToAreaType();
//            areaToAreaType.setArea(area);
//            areaToAreaType.setAreaType(primaryAreaTypes.get(c));
//            areaToAreaTypeCRUDRepository.save(areaToAreaType);
//            areaToAreaTypesToAdd.add(areaToAreaType);
//        });
        List<AreaToAreaType> areaToAreaTypesToRemove = new ArrayList<>();

//                areaToAreaTypes.stream()
//                .filter(a -> a.getAreaType() != null &&
//                        !primaryAreaTypeCodesAdd.contains(a.getAreaType().getCode()) &&
//                        primaryAreaTypeCodesDel.contains(a.getAreaType().getCode()))
//                .collect(Collectors.toList());

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
//        order.setOuz(ouz);
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
        String ouzNew = ""; //ouz == null ? order.getOuz() : ouz;
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
//        order.setOuz(ouzNew);
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
    public List<Long> setMedicalEmployeeOnArea(long areaId, List<AddMedicalEmployee> addEmployeesInput,
                                               List<ChangeMedicalEmployee> changeEmployeesInput) throws ContingentException {

        Validation validation = new Validation();

        //1 Система выполняет поиск в БД (AREAS) участка с переданным ИД. Участок найден, иначе возвращает ошибку
        Area area = areaCRUDRepository.findById(areaId).orElseThrow(() -> new ContingentException(
                validation.error(AreaErrorReason.AREA_NOT_FOUND, new ValidationParameter("areaId", areaId))));

        //2 Система проверяет, что участок не находится в архиве (AREAS.ARCHIVE = 0), иначе возвращает ошибку
        if (area.getArchived()) {
            throw new ContingentException(validation.error(
                    AreaErrorReason.AREA_IS_ARCHIVED, new ValidationParameter("areaId", areaId)));
        }


        List<Long> changeIds = changeEmployeesInput.stream().map(ChangeMedicalEmployee::getAssignmentId)
                .collect(Collectors.toList());
        List<AreaMedicalEmployee> changeEmployeesDb = new ArrayList<>();
        areaMedicalEmployeeCRUDRepository.findAllById(changeIds).forEach(changeEmployeesDb::add);

        //3
        if (area.getAreaType().getCode() != KindAreaTypeEnum.MILDLY_ASSOCIATED.getCode()
                && area.getAreaType().getCode() != KindAreaTypeEnum.TREATMENT_ROOM_ASSOCIATED.getCode()) {
            addEmployeesInput.stream().filter(empl -> !empl.isIsReplacement())
                    .forEach(empl -> validation.error(AreaErrorReason.AREA_NOT_RELATED_TO_MILDLY_ASSOCIATED));
            changeEmployeesDb.stream().filter(empl -> !empl.getReplacement())
                    .forEach(empl -> validation.error(AreaErrorReason.AREA_NOT_RELATED_TO_MILDLY_ASSOCIATED));
        }
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        List<AreaMedicalEmployee> areaEmployeesDb = areaMedicalEmployeeRepository.getEmployeesByAreaId(areaId);

        //4
        for (ChangeMedicalEmployee inputEmpl : changeEmployeesInput) {

            Optional<AreaMedicalEmployee> employee = areaEmployeesDb.stream().filter(
                    empl -> empl.getId().equals(inputEmpl.getAssignmentId())).findFirst();
            AreaMedicalEmployee emplDb = null;
            if (!employee.isPresent()) {
                validation.error(AreaErrorReason.EMPLOYEE_NOT_RELATED_TO_AREA,
                        new ValidationParameter("id", inputEmpl.getAssignmentId()));
            } else {
                emplDb = employee.get();
                //4.1
                if (inputEmpl.getEndDate() != null && inputEmpl.getEndDate().isBefore(LocalDate.now())
                        || employee.get().getArea().getId() != areaId) {
                    validation.error(AreaErrorReason.EMPLOYEE_NOT_RELATED_TO_AREA,
                            new ValidationParameter("id", inputEmpl.getAssignmentId()));
                }
            }

            //4.2
            if (inputEmpl.getEndDate() == null && inputEmpl.getStartDate() == null) {
                validation.error(AreaErrorReason.NOTHING_TO_CHANGE);
            }

            //4.3
            LocalDate startDate = inputEmpl.getStartDate() != null ? inputEmpl.getStartDate()
                    : emplDb != null ? emplDb.getStartDate() : null;
            LocalDate endDate = inputEmpl.getEndDate() != null ? inputEmpl.getEndDate()
                    : emplDb != null ? emplDb.getStartDate() : null;
            if (startDate != null && endDate != null && startDate.isAfter(endDate)) {
                validation.error(AreaErrorReason.START_DATE_IS_AFTER_END_DATE,
                        new ValidationParameter("startDate", startDate),
                        new ValidationParameter("endDate", endDate));
            }
        }
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        //5
        for (AddMedicalEmployee empl : addEmployeesInput) {
            //5.1
            if (empl.getStartDate().isBefore(LocalDate.now())) {
                validation.error(AreaErrorReason.START_DATE_IN_PAST,
                        new ValidationParameter("startDate", empl.getStartDate()));
            }

            //5.2
            Specialization specialization = specializationToPositionNomRepository.getSpecializationIdByPositionNomId(empl.getPositionId());

            //5.3
            if (specialization != null && !area.getAreaType().getSpecialization().getId().equals(specialization.getId())) {
                validation.error(AreaErrorReason.SPECIALIZATION_NOT_RELATED_TO_AREA,
                        new ValidationParameter("InputSpecialization", specialization.getTitle()),
                        new ValidationParameter("jobInfoId", empl.getMedicalEmployeeJobInfoId()),
                        new ValidationParameter("AreaSpecialization", area.getAreaType().getSpecialization().getTitle()));
            }

            //5.4
            List<AreaTypeMedicalPositions> positions = areaTypeMedicalPositionsRepository.getPositionsByAreaType(area.getAreaType().getCode());
            PositionNomClinic inputPosition = positionNomClinicCRUDRepository.findById(empl.getPositionId()).orElse(null);
            if (positions != null && positions.stream().anyMatch(pos -> pos.getPositionNomClinic().getId() != empl.getPositionId())) {
                validation.error(AreaErrorReason.POSITION_NOT_SET_FOR_AREA_TYPE,
                        new ValidationParameter("positionTitle", inputPosition != null ? inputPosition.getTitle() : null),
                        new ValidationParameter("jobInfoId", empl.getMedicalEmployeeJobInfoId()),
                        new ValidationParameter("areaTypeName", area.getAreaType().getName()));
            }
        }
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        //7.1
        if (areaEmployeesDb.size() > 1) {
            applyChanges(areaEmployeesDb, changeEmployeesInput);
            addNew(areaEmployeesDb, addEmployeesInput, area);
            for (int i = 0; i < areaEmployeesDb.size() - 1; i++) {
                AreaMedicalEmployee current = areaEmployeesDb.get(i);
                AreaMedicalEmployee next = areaEmployeesDb.get(i + 1);
                if (current.getMedicalEmployeeJobInfoId() != null
                        && current.getMedicalEmployeeJobInfoId().equals(next.getMedicalEmployeeJobInfoId())
                        && (current.getEndDate() == null || next.getStartDate().isBefore(current.getEndDate()))) {
                    validation.error(AreaErrorReason.JOB_ID_DATE_OVERLAP,
                            new ValidationParameter("jobInfoId", current.getMedicalEmployeeJobInfoId()),
                            new ValidationParameter("areaId", areaId),
                            new ValidationParameter("startDate1", current.getStartDate()),
                            new ValidationParameter("endDate1", current.getEndDate()),
                            new ValidationParameter("startDate2", next.getStartDate()),
                            new ValidationParameter("endDate2", next.getEndDate()));
                }
            }
            if (!validation.isSuccess()) {
                throw new ContingentException(validation);
            }
        }

        //7.2
        List<AreaMedicalEmployee> mainEmployees = areaMedicalEmployeeRepository.getMainEmployees(areaId);
        applyChanges(mainEmployees, changeEmployeesInput);
        addNew(mainEmployees, addEmployeesInput.stream()
                .filter(empl -> !empl.isIsReplacement()).collect(Collectors.toList()), area);
        if (area.getAreaType().getCode() == KindAreaTypeEnum.MILDLY_ASSOCIATED.getCode()) {
            Integer overlappingIndex = getEmployeeIndexWithOverlappingDates(mainEmployees);
            if (overlappingIndex != null) {
                throw new ContingentException(validation.error(AreaErrorReason.MAIN_EMPLOYEE_DATE_OVERLAP,
                        new ValidationParameter("specialization1", mainEmployees.get(overlappingIndex).getMedicalEmployeeJobInfoId()),
                        new ValidationParameter("specialization2", mainEmployees.get(overlappingIndex + 1).getMedicalEmployeeJobInfoId())));

            }
        }

        //7.3
        if (area.getAreaType().getCode() == KindAreaTypeEnum.MILDLY_ASSOCIATED.getCode()
                || area.getAreaType().getCode() == KindAreaTypeEnum.TREATMENT_ROOM_ASSOCIATED.getCode()) {
            //7.4
            List<Period> periodsWithoutMainEmpl = getPeriodsWithoutMainEmployee(mainEmployees);
            if (periodsWithoutMainEmpl.size() > 0) {
                //7.5
                List<AreaMedicalEmployee> replacementEmployees = areaMedicalEmployeeRepository.getReplacementEmployees(areaId);
                applyChanges(replacementEmployees, changeEmployeesInput);
                addNew(replacementEmployees, addEmployeesInput.stream()
                        .filter(AddMedicalEmployee::isIsReplacement).collect(Collectors.toList()), area);
                for (Period period : periodsWithoutMainEmpl) {
                    for (AreaMedicalEmployee empl : replacementEmployees) {
                        if (period.isInterceptWith(empl.getStartDate(), empl.getEndDate())) {
                            validation.error(AreaErrorReason.REPLACEMENT_WITHOUT_MAIN_EMPLOYEE,
                                    new ValidationParameter("startDate", period.getStartDate()),
                                    new ValidationParameter("endDate", period.getEndDate()));
                        }
                    }
                }
                if (!validation.isSuccess()) {
                    throw new ContingentException(validation);
                }
            }
        }

        //8.2
        applyChanges(changeEmployeesDb, changeEmployeesInput);
        //8.1
        addNew(changeEmployeesDb, addEmployeesInput, area);
        List<Long> result = new ArrayList<>();
        areaMedicalEmployeeCRUDRepository.saveAll(changeEmployeesDb).forEach(saved -> result.add(saved.getId()));
        return result;
    }

    @Override
    public void restoreArea(Long id) throws ContingentException {
        Validation validation = new Validation();

        Area area = areaChecker.checkAndGetArchivedArea(id, validation);
        areaChecker.checkAreaTypeIsNotPersonal(area.getAreaType(), validation);
        areaChecker.checkAreaExistsInMU(area.getMuId(), area.getAreaType().getCode(), area.getNumber(), area.getId(), validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        area.setArchived(false);
        area.setAutoAssignForAttach(false);
    }

    @Override
    public Long getNewAreaId() throws ContingentException {
        return areaRepository.getNextAreaId();
    }

    private List<Period> getPeriodsWithoutMainEmployee(List<AreaMedicalEmployee> mainEmployees) {
        mainEmployees.sort(Comparator.comparing(AreaMedicalEmployee::getStartDate));
        LocalDate currentDate = LocalDate.now();
        List<Period> periodsWithoutMainEmpl = new ArrayList<>();
        if (mainEmployees.size() == 0
                || (mainEmployees.size() == 1 && mainEmployees.get(0).getEndDate() == null)
                || mainEmployees.get(mainEmployees.size() - 1).getEndDate() == null) {
            return periodsWithoutMainEmpl;
        }
        if (mainEmployees.size() == 1 && mainEmployees.get(0).getEndDate() != null) {
            if (mainEmployees.get(0).getEndDate().isAfter(currentDate)) {
                periodsWithoutMainEmpl.add(new Period(mainEmployees.get(0).getEndDate(), null));
            } else {
                periodsWithoutMainEmpl.add(new Period(currentDate, null));
            }
            return periodsWithoutMainEmpl;
        }
        for (int i = 0; i < mainEmployees.size() - 1; i++) {
            AreaMedicalEmployee current = mainEmployees.get(i);
            AreaMedicalEmployee next = mainEmployees.get(i + 1);
            if (current.getEndDate() == null) {
                periodsWithoutMainEmpl.clear();
                return periodsWithoutMainEmpl;
            }
            if (next.getStartDate().isAfter(currentDate) && next.getStartDate().minusDays(1).isAfter(current.getEndDate())) {
                periodsWithoutMainEmpl.add(new Period(current.getEndDate().plusDays(1), next.getStartDate()));
            }
        }
        return periodsWithoutMainEmpl;
    }

    private Integer getEmployeeIndexWithOverlappingDates(List<AreaMedicalEmployee> sortedEmpl) {
        if (sortedEmpl.size() > 1) {
            for (int i = 0; i < sortedEmpl.size() - 1; i++) {
                if (sortedEmpl.get(i).getEndDate() != null
                        && sortedEmpl.get(i + 1).getStartDate().isBefore(sortedEmpl.get(i).getEndDate())) {
                    return i;
                }
            }
        }
        return null;
    }

    private void applyChanges(List<AreaMedicalEmployee> employees, List<ChangeMedicalEmployee> changeEmployees) {
        for (AreaMedicalEmployee empl : employees) {
            Optional<ChangeMedicalEmployee> optionalChangeEmpl = changeEmployees.stream().filter(
                    ch -> empl.getId().equals(ch.getAssignmentId())).findFirst();
            if (!optionalChangeEmpl.isPresent()) {
                continue;
            }
            ChangeMedicalEmployee changeEmpl = optionalChangeEmpl.get();
            if (changeEmpl.getStartDate() != null) {
                empl.setStartDate(changeEmpl.getStartDate());
            }

            if (changeEmpl.getEndDate() != null) {
                empl.setEndDate(changeEmpl.getEndDate());
            }
        }
    }

    private void addNew(List<AreaMedicalEmployee> employees, List<AddMedicalEmployee> addEmployees, Area area) {
        addEmployees.forEach(empl -> employees.add(new AreaMedicalEmployee(empl.getMedicalEmployeeJobInfoId(), area,
                empl.isIsReplacement(), empl.getStartDate(), empl.getEndDate(), empl.getSnils(), empl.getPositionId(),
                LocalDateTime.now(), LocalDateTime.now(), false, empl.getSubdivisionId())));
    }
}
