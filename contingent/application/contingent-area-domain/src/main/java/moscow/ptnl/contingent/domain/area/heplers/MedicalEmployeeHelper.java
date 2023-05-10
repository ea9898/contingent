package moscow.ptnl.contingent.domain.area.heplers;

import moscow.ptnl.contingent.domain.AreaErrorReason;
import moscow.ptnl.contingent.domain.area.entity.AreaMedicalEmployees;
import moscow.ptnl.contingent.domain.area.model.area.AddMedicalEmployee;
import moscow.ptnl.contingent.domain.area.model.area.ChangeMedicalEmployee;
import moscow.ptnl.contingent.error.Validation;
import moscow.ptnl.contingent.error.ValidationParameter;
import moscow.ptnl.contingent.nsi.domain.area.AreaType;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeMedicalPositions;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeSpecializations;
import moscow.ptnl.contingent.nsi.domain.area.MappingPositionCodeToOtherPosition;
import moscow.ptnl.contingent.nsi.domain.area.PositionCode;
import moscow.ptnl.contingent.nsi.domain.area.PositionNom;
import moscow.ptnl.contingent.nsi.domain.area.Specialization;
import moscow.ptnl.contingent.nsi.domain.repository.AreaTypeMedicalPositionsRepository;
import moscow.ptnl.contingent.nsi.domain.repository.AreaTypeSpecializationsRepository;
import moscow.ptnl.contingent.nsi.domain.repository.MappingPositionCodeToOtherPositionRepository;
import moscow.ptnl.contingent.nsi.domain.repository.PositionCodeRepository;
import moscow.ptnl.contingent.nsi.domain.repository.PositionNomRepository;
import moscow.ptnl.contingent.nsi.domain.repository.SpecializationRepository;
import moscow.ptnl.util.Strings;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

@Component
public class MedicalEmployeeHelper {

    @Autowired
    private AreaTypeSpecializationsRepository areaTypeSpecializationsRepository;

    @Autowired
    private AreaTypeMedicalPositionsRepository areaTypeMedicalPositionsRepository;

    @Autowired
    private PositionCodeRepository positionCodeRepository;

    @Autowired
    private PositionNomRepository positionNomRepository;

    @Autowired
    private SpecializationRepository specializationRepository;

    @Autowired
    private MappingPositionCodeToOtherPositionRepository mappingPositionCodeToOtherPositionRepository;

    public void checkChangeMedicalEmployee(ChangeMedicalEmployee inputEmployee, long areaId,
                                           List<AreaMedicalEmployees> areaEmployeesDb, Validation validation) {
        LocalDate now = LocalDate.now();
        //5.1
        if (inputEmployee.getEndDate() != null && inputEmployee.getEndDate().isBefore(now)) {
            validation.error(AreaErrorReason.EMPLOYEE_END_DATE_INCORRECT);
        }
        //5.2
        Optional<AreaMedicalEmployees> employee = areaEmployeesDb.stream()
                .filter(empl -> empl.getId().equals(inputEmployee.getAssignmentId()))
                .filter(e -> e.getEndDate() == null || !e.getEndDate().isBefore(now))
                .filter(e -> Objects.equals(e.getArea().getId(), areaId))
                .findFirst();
        AreaMedicalEmployees emplDb = employee.orElse(null);

        if (employee.isEmpty()) {
            validation.error(AreaErrorReason.EMPLOYEE_NOT_RELATED_TO_AREA,
                    new ValidationParameter("assignmentId", inputEmployee.getAssignmentId()));
        }
        if (inputEmployee.isIsError() == null || !inputEmployee.isIsError()) {
            //5.3.1
            LocalDate startDate = inputEmployee.getStartDate() != null ? inputEmployee.getStartDate()
                    : emplDb != null ? emplDb.getStartDate() : null;
            LocalDate endDate = inputEmployee.getEndDate() != null ? inputEmployee.getEndDate()
                    : emplDb != null ? emplDb.getEndDate() : null;
            if (startDate != null && endDate != null && startDate.isAfter(endDate)) {
                validation.error(AreaErrorReason.START_DATE_IS_AFTER_END_DATE,
                        new ValidationParameter("endDate", endDate),
                        new ValidationParameter("startDate", startDate));
            }
            //5.3.2
            if (Boolean.TRUE.equals(inputEmployee.getTempDuty()) &&
                    emplDb != null && !Boolean.TRUE.equals(emplDb.getReplacement())) {
                validation.error(AreaErrorReason.NOT_REPLACEMENT_EMPLOYEE);
            }
        }
    }

    public void checkAddMedicalEmployee(AddMedicalEmployee inputEmployee, AreaType areaType, Validation validation) {
        //6.1
        if (inputEmployee.getEndDate() != null && inputEmployee.getEndDate().isBefore(LocalDate.now())) {
            validation.error(AreaErrorReason.EMPLOYEE_END_DATE_INCORRECT);
        }
        //6.2
        if (Boolean.TRUE.equals(inputEmployee.getTempDuty()) && !inputEmployee.isReplacement()) {
            validation.error(AreaErrorReason.NOT_REPLACEMENT_EMPLOYEE);
        }
        //6.4
        if (inputEmployee.getStartDate().isBefore(LocalDate.now())) {
            validation.error(AreaErrorReason.START_DATE_IN_PAST,
                    new ValidationParameter("startDate", inputEmployee.getStartDate()));
        }
        if (inputEmployee.getEndDate() != null && inputEmployee.getStartDate().isAfter(inputEmployee.getEndDate())) {
            validation.error(AreaErrorReason.START_DATE_IS_AFTER_END_DATE,
                    new ValidationParameter("endDate", inputEmployee.getEndDate()),
                    new ValidationParameter("startDate", inputEmployee.getStartDate()));
        }
        //6.5
        List<AreaTypeSpecializations> areaTypeSpecializations = areaTypeSpecializationsRepository.findByAreaTypeCode(areaType);
        final List<PositionCode> positionsCode = new ArrayList<>();
        boolean positionCodeFromSUPP = Strings.isNumberWith4Digits(inputEmployee.getPositionCode());
        //6.5.1.
        if (positionCodeFromSUPP) {
            //ЕСЛИ код должности медработника числовой (= медработник ведется в СУПП)
            List<MappingPositionCodeToOtherPosition> mappingPositions =
                    mappingPositionCodeToOtherPositionRepository.findByPositionSuppCode(inputEmployee.getPositionCode());

            if (!mappingPositions.isEmpty()) {
                positionsCode.addAll(positionCodeRepository.getByGlobalIds(mappingPositions.stream()
                        .map(MappingPositionCodeToOtherPosition::getPositionCodeId)
                        .collect(Collectors.toSet())));
            }
        } else {
            //ИНАЧЕ (код должности медработника символьный) определяет ИД кода должности из СВМР.2
            positionCodeRepository.getByCode(inputEmployee.getPositionCode())
                    .ifPresent(positionsCode::add);
        }
        if (positionsCode.isEmpty()) {
            validation.error(AreaErrorReason.POSITION_CODE_NOT_FOUND,
                    new ValidationParameter("positionCode", inputEmployee.getPositionCode()));

        }
        //6.5.2.
        final List<PositionNom> positionsNom = positionsCode.isEmpty() ? Collections.emptyList() :
                positionNomRepository.findByPositionCodeIds(positionsCode.stream()
                        .map(PositionCode::getGlobalId)
                        .collect(Collectors.toList())
                ).stream()
                        .filter(p -> positionCodeFromSUPP || p.getEndDate() == null)
                        .collect(Collectors.toList());
        if (positionsNom.isEmpty()) {
            validation.error(AreaErrorReason.SPECIALIZATION_IS_NOT_SPECIFIED,
                    new ValidationParameter("positionCode", inputEmployee.getPositionCode()));
        }
        //6.6
        List<String> specializations = positionsNom.stream()
                .map(PositionNom::getSpecialization)
                .filter(Objects::nonNull)
                .distinct()
                .filter(item -> item.getArchived() != null && item.getArchived().equals(Boolean.FALSE))
                .map(Specialization::getCode)
                .filter(Objects::nonNull)
                .collect(Collectors.toList());

        if (specializations.isEmpty()) {
            validation.error(AreaErrorReason.SPECIALIZATION_CODE_IS_NOT_DEFINED,
                    new ValidationParameter("positionCode", positionsNom.stream()
                            .map(i -> i.getGlobalId().toString())
                            .collect(Collectors.joining(", "))));
        }
        //6.7
        if (areaTypeSpecializations.stream().filter(item -> item.getArchived() != null && item.getArchived().equals(Boolean.FALSE))
                .noneMatch(ats -> specializations.contains(ats.getSpecializationCode()))) {

            validation.error(AreaErrorReason.SPECIALIZATION_NOT_RELATED_TO_AREA,
                    new ValidationParameter("SpecializationTitle", positionsNom.stream()
                            .map(PositionNom::getSpecialization)
                            .filter(Objects::nonNull)
                            .distinct()
                            .map(Specialization::getTitle)
                            .collect(Collectors.joining(","))),
                    new ValidationParameter("jobInfoId", inputEmployee.getMedicalEmployeeJobInfoId()),
                    new ValidationParameter("AreaSpecializationTitles", areaTypeSpecializations.stream()
                            .map(AreaTypeSpecializations::getSpecializationCode)
                            .filter(Objects::nonNull)
                            .distinct()
                            .map(specializationRepository::getByCode)
                            .filter(Objects::nonNull)
                            .map(Specialization::getTitle)
                            .collect(Collectors.joining(", "))));
        }
        //6.8
        List<AreaTypeMedicalPositions> positions = areaTypeMedicalPositionsRepository.getPositionsByAreaType(areaType.getCode());

        if (!positions.isEmpty() && positions.stream().noneMatch(p -> positionsCode.stream()
                .map(PositionCode::getCode)
                .anyMatch(c -> Objects.equals(c, p.getPositionCode().getCode()))
        )) {
            validation.error(AreaErrorReason.POSITION_NOT_SET_FOR_AREA_TYPE,
                    new ValidationParameter("positionTitle", positionsNom.stream()
                            .distinct()
                            .map(PositionNom::getTitle)
                            .collect(Collectors.joining(","))),
                    new ValidationParameter("jobInfoId", inputEmployee.getMedicalEmployeeJobInfoId()),
                    new ValidationParameter("areaTypeTitle", areaType.getTitle()));
        }
    }
}
