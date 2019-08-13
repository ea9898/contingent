package moscow.ptnl.contingent.esuInputTasks;

import moscow.ptnl.contingent.area.entity.area.Area;
import moscow.ptnl.contingent.area.entity.area.AreaMedicalEmployees;
import moscow.ptnl.contingent.area.entity.area.MoAvailableAreaTypes;
import moscow.ptnl.contingent.area.entity.nsi.AreaType;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeKindEnum;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeSpecializations;
import moscow.ptnl.contingent.area.entity.nsi.PositionNom;
import moscow.ptnl.contingent.repository.area.AreaCRUDRepository;
import moscow.ptnl.contingent.repository.area.AreaMedicalEmployeeCRUDRepository;
import moscow.ptnl.contingent.repository.area.AreaMedicalEmployeeRepository;
import moscow.ptnl.contingent.repository.area.AreaRepository;
import moscow.ptnl.contingent.repository.area.MoAvailableAreaTypesRepository;
import moscow.ptnl.contingent.repository.nsi.AreaTypeSpecializationsRepository;
import moscow.ptnl.contingent.repository.nsi.PositionNomRepository;
import moscow.ptnl.contingent.service.esu.EsuService;
import moscow.ptnl.contingent.util.EsuTopicsEnum;
import moscow.ptnl.contingent2.rmr.event.JeChangeDateEnd;
import moscow.ptnl.contingent2.rmr.event.JeCreate;
import moscow.ptnl.contingent2.rmr.event.JobExecutionInfoMsg;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * К_УУ_ЕСУ_2
 */
@Component
@Qualifier("jobExecutionInfoMsgTopicTask")
public class JobExecutionInfoMsgTopicTask extends BaseTopicTask<JobExecutionInfoMsg> {

    private static final String XSD_PATH = "META-INF/xsd/esu/jobExecutionInfoMsg.xsd";

    //Todo перенести в настройки или еще куда?
    private static final Set<Long> SPECIALIZATION_CODES_ONCOLOGY = new HashSet<>(Arrays.asList(19L, 41L));

    @Autowired
    private AreaCRUDRepository areaCRUDRepository;

    @Autowired
    private AreaRepository areaRepository;

    @Autowired
    private PositionNomRepository positionNomRepository;

    @Autowired
    private AreaTypeSpecializationsRepository areaTypeSpecializationsRepository;

    @Autowired
    private MoAvailableAreaTypesRepository moAvailableAreaTypesRepository;

    @Autowired
    private AreaMedicalEmployeeCRUDRepository areaMedicalEmployeeCRUDRepository;

    @Autowired
    private AreaMedicalEmployeeRepository areaMedicalEmployeeRepository;

    @Autowired
    private EsuService esuService;

    public JobExecutionInfoMsgTopicTask() {
        super(EsuTopicsEnum.JOB_EXECUTION_INFO_MSG, XSD_PATH, JobExecutionInfoMsg.class);
    }

    @Override
    protected String getEventId(JobExecutionInfoMsg event) {
        return event.getActionId();
    }

    @Override
    public void processMessage(JobExecutionInfoMsg event) {
        if (event.getJeCreate() != null) {
            createAreaWithMedicalEmployee(event.getJeCreate());
        }
        if (event.getJeChangeDateEnd() != null) {
            changeMedicalEmployeeEndDate(event.getJeChangeDateEnd());
        }
    }

    private void changeMedicalEmployeeEndDate(JeChangeDateEnd jeChangeDateEnd) {
        //6.1
        List<AreaMedicalEmployees> employees = areaMedicalEmployeeRepository.findEmployees(jeChangeDateEnd.getId(), false).stream()
                .filter(e -> e.getArea().isActual() &&
                        e.getArea().getAreaType() != null &&
                        e.getArea().getAreaType().getAreaTypeKind() != null &&
                        Objects.equals(e.getArea().getAreaType().getAreaTypeKind().getCode(), AreaTypeKindEnum.PERSONAL.getCode()))
                .collect(Collectors.toList());

        if (employees.isEmpty()) {
            throw new RuntimeException("Для обновления даты окончания ИДМР участок не найден");
        }
        //6.2
        employees.forEach(e -> {
            e.setEndDate(EsuInputTaskHelper.convertToLocalDate(jeChangeDateEnd.getEnd()));
            e.setUpdateDate(LocalDateTime.now());
        });
    }

    private void createAreaWithMedicalEmployee(JeCreate jeCreate) {
        if (jeCreate.getPositionNom() == null || jeCreate.getDepartment() == null || jeCreate.getDepartment().getOrganization() == null) {
            //Todo уточнить текст ошибки
            throw new RuntimeException("Некорректные данные JeCreate");
        }
        Long moId = jeCreate.getDepartment().getOrganization().getId();
        //2.
        PositionNom positionNom = positionNomRepository.getByCode(jeCreate.getPositionNom().getCode()).get();
        //3.
        Optional<AreaTypeSpecializations> areaTypeSpecialization = areaTypeSpecializationsRepository.findBySpecializationCode(
                positionNom.getSpecialization().getCode()).stream()
                .filter(a -> Objects.equals(a.getAreaType().getAreaTypeKind().getCode(), AreaTypeKindEnum.PERSONAL.getCode()))
                .findFirst();

        if (!areaTypeSpecialization.isPresent() || areaTypeSpecialization.get().getAreaType() == null) {
            throw new RuntimeException("Специализация ИДМР не соответствует именному виду участка");
        }
        Long specializationCode = positionNom.getSpecialization().getCode();
        AreaType areaType = areaTypeSpecialization.get().getAreaType();
        //5.1
        if (!SPECIALIZATION_CODES_ONCOLOGY.contains(specializationCode)) {
            //5.1.1
            List<MoAvailableAreaTypes> areaTypes = moAvailableAreaTypesRepository.findAreaTypes(moId);

            if (areaTypes.isEmpty()) {
                throw new RuntimeException("Не найден ни один разрешенный первичный тип участка");
            }
            //5.1.2
            List<AreaTypeSpecializations> primarySpecializations = areaTypeSpecializationsRepository.findByAreaTypeCode(
                    areaTypes.stream()
                            .map(MoAvailableAreaTypes::getAreaType)
                            .collect(Collectors.toList())
            );
            //5.1.3
            if (primarySpecializations.stream().anyMatch(s -> Objects.equals(s.getSpecializationCode(), specializationCode))) {
                throw new RuntimeException("Специализация ИДМР совпадает со специализацией разрешенного первичного типа участка МО");
            }
            //5.2
            List<Area> areas = areaRepository.findAreas(moId, null, areaType.getCode(), null, true);

            if (areas.stream()
                    .flatMap(a -> a.getActualMainMedicalEmployees().stream())
                    .anyMatch(a -> Objects.equals(a.getMedicalEmployeeJobId(), jeCreate.getId()))) {
                throw new RuntimeException("Для данного ИДМР участок уже существует");
            }
            Area area;
            //5.3
            Optional<AreaMedicalEmployees> areaMedicalEmployee = areas.stream()
                    .filter(a -> a.getMuId() == null)
                    .flatMap(a -> a.getActualMainMedicalEmployees().stream())
                    .filter(a -> !Objects.equals(a.getMedicalEmployeeJobId(), jeCreate.getId()) &&
                            Objects.equals(a.getSnils(), jeCreate.getEmployee().getSnils()))
                    .findFirst();

            if (areaMedicalEmployee.isPresent() && areaMedicalEmployee.get().getArea() != null) {
                //АС.1
                area = areaMedicalEmployee.get().getArea();
            }
            else {
                //5.4
                try {
                    area = createArea(moId, areaType);
                } catch (Throwable e) {
                    throw new RuntimeException("Ошибка создания участка: " + e.getMessage(), e);
                }
            }
            try {
                //5.5
                createAreaMedicalEmployee(area, positionNom, jeCreate);
            }
            catch (Throwable e) {
                throw new RuntimeException("Ошибка добавления МР на участок: " + e.getMessage(), e);
            }
        }
    }

    private Area createArea(Long moId, AreaType areaType) {
        Area area = new Area();
        area.setMoId(moId);
        area.setAreaType(areaType);
        area.setCreateDate(LocalDateTime.now());
        area.setUpdateDate(LocalDateTime.now());
        areaCRUDRepository.save(area);

        return area;
    }

    private AreaMedicalEmployees createAreaMedicalEmployee(Area area, PositionNom positionNom, JeCreate jeCreate) {
        AreaMedicalEmployees medicalEmployee = new AreaMedicalEmployees();
        medicalEmployee.setArea(area);
        medicalEmployee.setMedicalEmployeeJobId(jeCreate.getId());
        medicalEmployee.setReplacement(false);
        medicalEmployee.setSnils(jeCreate.getEmployee().getSnils());
//        medicalEmployee.setPositionNomCode(positionNom.getCode()); CONTINGENT2-280
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