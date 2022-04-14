package moscow.ptnl.contingent.esuInputTasks;

import moscow.ptnl.contingent.domain.area.entity.Area;
import moscow.ptnl.contingent.domain.area.entity.AreaMedicalEmployees;
import moscow.ptnl.contingent.domain.area.entity.MoAvailableAreaTypes;
import moscow.ptnl.contingent.infrastructure.service.setting.SettingService;
import moscow.ptnl.contingent.nsi.domain.area.AreaType;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeKindEnum;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeMedicalPositions;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeSpecializations;
import moscow.ptnl.contingent.nsi.domain.area.PositionCode;
import moscow.ptnl.contingent.nsi.domain.area.PositionNom;
import moscow.ptnl.contingent.nsi.domain.area.Specialization;
import moscow.ptnl.contingent.nsi.domain.area.SpecializationEnum;
import moscow.ptnl.contingent.nsi.domain.repository.AreaTypeMedicalPositionsRepository;
import moscow.ptnl.contingent.nsi.domain.repository.AreaTypeSpecializationsRepository;
import moscow.ptnl.contingent.nsi.domain.repository.PositionCodeRepository;
import moscow.ptnl.contingent.nsi.domain.repository.PositionNomRepository;
import moscow.ptnl.contingent.nsi.repository.SpecializationCRUDRepository;
import moscow.ptnl.contingent.repository.area.AreaCRUDRepository;
import moscow.ptnl.contingent.repository.area.AreaMedicalEmployeeCRUDRepository;
import moscow.ptnl.contingent.domain.area.repository.AreaMedicalEmployeeRepository;
import moscow.ptnl.contingent.domain.area.repository.AreaRepository;
import moscow.ptnl.contingent.domain.area.repository.MoAvailableAreaTypesRepository;
import moscow.ptnl.contingent2.rmr.event.JeChangeDateEnd;
import moscow.ptnl.contingent2.rmr.event.JeCreate;
import moscow.ptnl.contingent2.rmr.event.JobExecutionInfoMsg;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

/**
 * К_УУ_ЕСУ_2
 */
@Component
@Qualifier("jobExecutionInfoMsgTopicTask")
public class JobExecutionInfoMsgTopicTask extends BaseTopicTask<JobExecutionInfoMsg> {

    private static final String XSD_PATH = "META-INF/xsd/esu/jobExecutionInfoMsg.xsd";

    @Value("${esu.consumer.topic.job.execution.info.msg}")
    private String jobExecutionInfoMsgTopicName;

    @Autowired
    private AreaCRUDRepository areaCRUDRepository;

    @Autowired
    private AreaRepository areaRepository;

    @Autowired
    private PositionNomRepository positionNomRepository;

    @Autowired
    private PositionCodeRepository positionCodeRepository;

    @Autowired
    private SpecializationCRUDRepository specializationCRUDRepository;

    @Autowired
    private AreaTypeSpecializationsRepository areaTypeSpecializationsRepository;

    @Autowired
    private MoAvailableAreaTypesRepository moAvailableAreaTypesRepository;

    @Autowired
    private AreaMedicalEmployeeCRUDRepository areaMedicalEmployeeCRUDRepository;

    @Autowired
    private AreaMedicalEmployeeRepository areaMedicalEmployeeRepository;

    @Autowired
    private AreaTypeMedicalPositionsRepository areaTypeMedicalPositionsRepository;

    @Autowired
    private SettingService settingService;

    public JobExecutionInfoMsgTopicTask() {
        super(null, JobExecutionInfoMsg.class);
    }

    @Override
    public String getTopicName() {
        return jobExecutionInfoMsgTopicName;
    }

    @Override
    protected String getEventId(JobExecutionInfoMsg event) {
        return event.getActionId();
    }

    @Override
    @Transactional(propagation = Propagation.MANDATORY)
    public void processMessage(JobExecutionInfoMsg event) {
        //2
        if (event.getJeCreate() != null) {
            createAreaWithMedicalEmployee(event.getJeCreate());
        }
        //3
        if (event.getJeChangeDateEnd() != null) {
            changeMedicalEmployeeEndDate(event.getJeChangeDateEnd());
        }
        //4
        if (event.getJeChange() != null || event.getJeRemove() != null) {
            throw new RuntimeException("Отсутствуют данные для обработки");
        }
    }

    private void changeMedicalEmployeeEndDate(JeChangeDateEnd jeChangeDateEnd) {
        //3.1
        List<AreaMedicalEmployees> employees = areaMedicalEmployeeRepository.findEmployees(jeChangeDateEnd.getId(), false).stream()
                .filter(e -> e.getArea().isActual() &&
                        e.getArea().getAreaType() != null &&
                        e.getArea().getAreaType().getAreaTypeKind() != null &&
                        Objects.equals(e.getArea().getAreaType().getAreaTypeKind().getCode(), AreaTypeKindEnum.PERSONAL.getCode()))
                .collect(Collectors.toList());

        if (employees.isEmpty()) {
            throw new RuntimeException("Для обновления даты окончания ИДМР участок не найден");
        }
        //3.2
        employees.forEach(e -> {
            e.setEndDate(EsuInputTaskHelper.convertToLocalDate(jeChangeDateEnd.getEnd()));
            e.setUpdateDate(LocalDateTime.now());
        });
    }

    private void createAreaWithMedicalEmployee(JeCreate jeCreate) {

        //2.1
        StringBuilder errorParameters = new StringBuilder();
        if (jeCreate.getDepartment() == null) {
            errorParameters.append("jeCreate.department, ");
        } else if (jeCreate.getDepartment().getOrganization() == null) {
            errorParameters.append("jeCreate.department.organization, ");
        }
        if (jeCreate.getEmployee() == null) {
            errorParameters.append("jeCreate.employee, ");
        } else if (jeCreate.getEmployee().getSnils() == null) {
            errorParameters.append("jeCreate.employee.snils, ");
        }
        if (jeCreate.getPositionNom() == null) {
            errorParameters.append("jeCreate.positionNom;");
        } else if (jeCreate.getPositionNom().getPositionNom() == null) {
            errorParameters.append("jeCreate.positionNom.positionNom;");
        } else {
            if (jeCreate.getPositionNom().getPositionNom().getCode() == null) {
                errorParameters.append("jeCreate.positionNom.positionNom.code;");
            }
            if (jeCreate.getPositionNom().getPositionNom().getTitle() == null) {
                errorParameters.append("jeCreate.positionNom.positionNom.title;");
            }
            if (jeCreate.getPositionNom().getPositionNom().getSpecialization() != null) {
                if (jeCreate.getPositionNom().getPositionNom().getSpecialization().getCode() == null) {
                    errorParameters.append("jeCreate.positionNom.positionNom.specialization.code;");
                }
                if (jeCreate.getPositionNom().getPositionNom().getSpecialization().getTitle() == null) {
                    errorParameters.append("jeCreate.positionNom.positionNom.specialization.title;");
                }
            }
        }
        if (errorParameters.length() != 0) {
            throw new RuntimeException("Отсутствуют параметры: " + errorParameters.toString());
        }

        //2.2.1
        Optional<PositionCode> positionCode = positionCodeRepository.getByCode(jeCreate.getPositionNom().getPositionNom().getCode());
        if (!positionCode.isPresent()) {
            throw new RuntimeException("Не найден ИД кода должности");
        }

        Optional<PositionNom> positionNom = positionNomRepository.getByPositionCodeId(positionCode.get().getGlobalId());
        if (!positionNom.isPresent() || positionNom.get().getSpecialization() == null) {
            throw new RuntimeException("Не найден ИД специализации");
        }

        //2.2.2
        Optional<Specialization> specialization = specializationCRUDRepository.findById(positionNom.get().getSpecialization().getGlobalId());
        if (!specialization.isPresent() || specialization.get().getCode() == null) {
            throw new RuntimeException("Не найден код специализации");
        }

        //2.3
        Optional<AreaTypeSpecializations> areaTypeSpecialization = areaTypeSpecializationsRepository.findBySpecializationCode(
                specialization.get().getCode()).stream()
                .filter(a -> Objects.equals(a.getAreaType().getAreaTypeKind().getCode(), AreaTypeKindEnum.PERSONAL.getCode()))
                .findFirst();
        if (!areaTypeSpecialization.isPresent()) {
            throw new RuntimeException("Специализация ИДМР не соответствует именному виду участка");
        }
        Long moId = jeCreate.getDepartment().getOrganization().getId();

        //2.4
        if (!settingService.getPar20().contains(specialization.get().getCode())) {
            //2.4.1
            List<MoAvailableAreaTypes> moAvailableAreaTypes = moAvailableAreaTypesRepository.findAreaTypes(moId);

            if (moAvailableAreaTypes.isEmpty()) {
                throw new RuntimeException("Не найден ни один разрешенный тип участка первичного класса");
            }

            //2.4.2
            List<AreaTypeSpecializations> primarySpecializationsTotal = new ArrayList<>();
            StringBuilder errorAreaTypeCodes = new StringBuilder();
            for (MoAvailableAreaTypes moAvailableAreaType : moAvailableAreaTypes) {
                List<AreaTypeSpecializations> primarySpecializations = areaTypeSpecializationsRepository.findByAreaTypeCode(
                        moAvailableAreaType.getAreaType());
                if (primarySpecializations.isEmpty()) {
                    if (errorAreaTypeCodes.length() == 0) {
                        errorAreaTypeCodes.append(moAvailableAreaType.getAreaType().getCode());
                    } else {
                        errorAreaTypeCodes.append(", ").append(moAvailableAreaType.getAreaType().getCode());
                    }
                } else {
                    primarySpecializationsTotal.addAll(primarySpecializations);
                }
            }
            if (errorAreaTypeCodes.length() != 0) {
                throw new RuntimeException(String.format("Для типов участков %s специализация не указана",
                        errorAreaTypeCodes.toString()));
            }

            //2.4.3
            if (primarySpecializationsTotal.stream().anyMatch(s -> Objects.equals(s.getSpecializationCode(), specialization.get().getCode()))) {
                throw new RuntimeException("Специализация ИДМР совпадает со специализацией разрешенного для МО типа участка первичного класса");
            }
        }
        //2.5
        AreaType areaType = areaTypeSpecialization.get().getAreaType();

        //2.6
        List<AreaTypeMedicalPositions> areaTypeMedicalPositions =
                areaTypeMedicalPositionsRepository.getPositionsByAreaType(areaType.getCode());
        if (areaTypeMedicalPositions != null && areaTypeMedicalPositions.stream().noneMatch(pos -> pos.getPositionCode().getCode().equals(jeCreate.getPositionNom().getPositionNom().getCode()))) {
            throw new RuntimeException(String.format("Должность ИДМР не разрешена для типа участка %s", areaType.getTitle()));
        }

        //2.7
        List<Area> areas = areaRepository.findAreas(moId, null, areaType.getCode(), null, true);
        if (areas.stream()
                .flatMap(a -> a.getActualMainMedicalEmployees().stream())
                .anyMatch(a -> Objects.equals(a.getMedicalEmployeeJobId(), jeCreate.getId()))) {
            throw new RuntimeException("Для данного ИДМР участок уже существует");
        }
        Area area;

        //2.8
        Optional<AreaMedicalEmployees> areaMedicalEmployee = areas.stream()
                .filter(a -> a.getMuId() == null)
                .flatMap(a -> a.getActualMainMedicalEmployees().stream())
                .filter(a -> !Objects.equals(a.getMedicalEmployeeJobId(), jeCreate.getId()) &&
                        Objects.equals(a.getSnils(), jeCreate.getEmployee().getSnils()))
                .findFirst();

        if (areaMedicalEmployee.isPresent() && areaMedicalEmployee.get().getArea() != null) {
            //AC.1
            area = areaMedicalEmployee.get().getArea();
        }
        else {
            //2.9
            try {
                area = createArea(moId, areaType);
            } catch (Throwable e) {
                throw new RuntimeException("Ошибка создания участка: " + e.getMessage(), e);
            }
        }
        try {
            //2.10
            createAreaMedicalEmployee(area, positionCode.get(), jeCreate);
        }
        catch (Throwable e) {
            throw new RuntimeException("Ошибка добавления МР на участок: " + e.getMessage(), e);
        }
    }

    private Area createArea(Long moId, AreaType areaType) {
        Area area = new Area();
        area.setMoId(moId);
        area.setAreaType(areaType);
        LocalDateTime now = LocalDateTime.now();
        area.setCreateDate(now);
        area.setUpdateDate(now);
        areaCRUDRepository.save(area);

        return area;
    }

    private AreaMedicalEmployees createAreaMedicalEmployee(Area area, PositionCode positionCode, JeCreate jeCreate) {
        AreaMedicalEmployees medicalEmployee = new AreaMedicalEmployees();
        medicalEmployee.setArea(area);
        medicalEmployee.setMedicalEmployeeJobId(jeCreate.getId());
        medicalEmployee.setReplacement(false);
        medicalEmployee.setSnils(jeCreate.getEmployee().getSnils());
        medicalEmployee.setPositionCode(positionCode.getCode());
        medicalEmployee.setSubdivisionId(jeCreate.getDepartment().getId());
        medicalEmployee.setStartDate(EsuInputTaskHelper.convertToLocalDate(jeCreate.getPeriod().getStart()));
        medicalEmployee.setEndDate(jeCreate.getPeriod().getEnd().isNil() ? null :
                EsuInputTaskHelper.convertToLocalDate(jeCreate.getPeriod().getEnd().getValue()));
        medicalEmployee.setCreateDate(LocalDateTime.now());
        medicalEmployee.setUpdateDate(LocalDateTime.now());
        areaMedicalEmployeeCRUDRepository.save(medicalEmployee);

        return medicalEmployee;
    }
}
