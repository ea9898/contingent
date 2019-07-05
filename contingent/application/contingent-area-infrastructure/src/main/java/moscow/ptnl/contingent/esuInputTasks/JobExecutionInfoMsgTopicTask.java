package moscow.ptnl.contingent.esuInputTasks;

import moscow.ptnl.contingent.area.entity.area.Area;
import moscow.ptnl.contingent.area.entity.area.AreaMedicalEmployees;
import moscow.ptnl.contingent.area.entity.area.MoAvailableAreaTypes;
import moscow.ptnl.contingent.area.entity.nsi.AreaType;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeSpecializations;
import moscow.ptnl.contingent.area.entity.nsi.PositionNom;
import moscow.ptnl.contingent.repository.area.AreaCRUDRepository;
import moscow.ptnl.contingent.repository.area.AreaMedicalEmployeeCRUDRepository;
import moscow.ptnl.contingent.repository.area.AreaRepository;
import moscow.ptnl.contingent.repository.area.MoAvailableAreaTypesRepository;
import moscow.ptnl.contingent.repository.nsi.AreaTypeSpecializationsRepository;
import moscow.ptnl.contingent.repository.nsi.AreaTypesCRUDRepository;
import moscow.ptnl.contingent.repository.nsi.PositionNomRepository;
import moscow.ptnl.contingent.service.esu.EsuService;
import moscow.ptnl.contingent.util.EsuTopicsEnum;
import moscow.ptnl.contingent2.rmr.event.JeChangeDateEnd;
import moscow.ptnl.contingent2.rmr.event.JeCreate;
import moscow.ptnl.contingent2.rmr.event.JobExecutionInfoMsg;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;

import java.lang.invoke.MethodHandles;
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
    private AreaTypesCRUDRepository areaTypesCRUDRepository;

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
        if (event.getJeCreate() == null || event.getJeCreate().getPositionNom() == null || event.getJeCreate().getDepartment() == null) {
            //Todo уточнить код ошибки
            throw new RuntimeException("Некорректные данные JeCreate");
        }
        Long moId = event.getJeCreate().getDepartment().getOrganization().getId();
        //2.
        PositionNom positionNom = positionNomRepository.getByCode(event.getJeCreate().getPositionNom().getCode()).get();
        //3.
        Optional<AreaTypeSpecializations> areaTypeSpecialization = areaTypeSpecializationsRepository.findBySpecializationCode(
                positionNom.getSpecialization().getCode()).stream()
                //Todo перенести код AreaTypeKind в настройки ?
                .filter(a -> Objects.equals(a.getAreaType().getAreaTypeKind().getCode(), 4L))
                .findFirst();

        if (!areaTypeSpecialization.isPresent() || areaTypeSpecialization.get().getAreaType() == null) {
            throw new RuntimeException("Специализация ИДМР не соответствует именному виду участка");
        }
        if (event.getJeCreate() != null && event.getJeCreate().getDepartment() != null) {
            createAreaWithMedicalEmployee(event.getJeCreate(), moId, positionNom, areaTypeSpecialization.get().getAreaType());
        }
        if (event.getJeChangeDateEnd() != null) {
            changeMedicalEmployeeEndDate(event.getJeChangeDateEnd(), moId, areaTypeSpecialization.get().getAreaType(), event.getJeCreate().getId());
        }
    }

    private void changeMedicalEmployeeEndDate(JeChangeDateEnd jeChangeDateEnd, Long moId, AreaType areaType, Long jobId) {
        //6.1
        List<Area> areas = areaRepository.findAreas(moId, null, areaType.getCode(), null, true);

        Optional<AreaMedicalEmployees> areaMedicalEmployee = areas.stream()
                .flatMap(a -> a.getActualMainMedicalEmployees().stream())
                .filter(a -> Objects.equals(a.getMedicalEmployeeJobInfoId(), jobId))
                .findFirst();

        if (!areaMedicalEmployee.isPresent()) {
            throw new RuntimeException("Для обновления даты окончания ИДМР участок не найден");
        }
        //6.2
        areaMedicalEmployee.get().setEndDate(EsuInputTaskHelper.convertToLocalDate(jeChangeDateEnd.getEnd()));
        areaMedicalEmployee.get().setUpdateDate(LocalDateTime.now());
    }

    private void createAreaWithMedicalEmployee(JeCreate jeCreate, Long moId, PositionNom positionNom, AreaType areaType) {
        Long specializationCode = positionNom.getSpecialization().getCode();
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
                    .anyMatch(a -> Objects.equals(a.getMedicalEmployeeJobInfoId(), jeCreate.getId()))) {
                throw new RuntimeException("Для данного ИДМР участок уже существует");
            }
            Area area;
            //5.3
            Optional<AreaMedicalEmployees> areaMedicalEmployee = areas.stream()
                    .filter(a -> a.getMuId() == null)
                    .flatMap(a -> a.getActualMainMedicalEmployees().stream())
                    .filter(a -> !Objects.equals(a.getMedicalEmployeeJobInfoId(), jeCreate.getId()) &&
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
        medicalEmployee.setMedicalEmployeeJobInfoId(jeCreate.getId());
        medicalEmployee.setReplacement(false);
        medicalEmployee.setSnils(jeCreate.getEmployee().getSnils());
        medicalEmployee.setPositionNom(positionNom);
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