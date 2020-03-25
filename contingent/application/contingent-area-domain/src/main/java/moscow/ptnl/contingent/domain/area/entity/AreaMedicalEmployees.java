package moscow.ptnl.contingent.domain.area.entity;

import moscow.ptnl.contingent.domain.converter.BooleanIntegerConverter;
import moscow.ptnl.contingent.domain.history.ServiceName;
import moscow.ptnl.contingent.domain.history.meta.Journalable;
import moscow.ptnl.contingent.domain.history.meta.LogIt;
import moscow.ptnl.contingent.nsi.domain.area.PositionCode;

import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.validation.constraints.Size;
import java.io.Serializable;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Objects;
import java.util.Optional;

@Entity @Journalable(ServiceName.AREA)
@Table(name = "AREA_MEDICAL_EMPLOYEES")
@SequenceGenerator(name = "seq_area_medical_employee", sequenceName = "seq_area_medical_employee", allocationSize=1)
public class AreaMedicalEmployees implements Serializable, Cloneable {

    private static final long serialVersionUID = 4435222693561566689L;

    @Id
    @GeneratedValue(strategy= GenerationType.SEQUENCE, generator="seq_area_medical_employee")
    @Column(name = "ID", unique = true, nullable = false)
    private Long id;

    @LogIt
    @Column(name = "MEDICAL_EMPLOYEE_JOB_ID")
    private Long medicalEmployeeJobId;

    @LogIt
    @JoinColumn(name = "AREA_ID")
    @ManyToOne(fetch = FetchType.LAZY)
    private Area area;

    @LogIt
    @Column(name = "IS_REPLACEMENT")
    @Convert(converter = BooleanIntegerConverter.class)
    private Boolean replacement;

    @LogIt
    @Column(name = "START_DATE")
    private LocalDate startDate;

    @LogIt
    @Column(name = "END_DATE")
    private LocalDate endDate;

    @LogIt
    @Column(name = "SNILS")
    @Size(max = 20)
    private String snils;

    @LogIt
    @Column(name = "POSITION_CODE")
    private String positionCode;

    @LogIt
    @Column(name = "CREATE_DATE", nullable = false)
    private LocalDateTime createDate;

    @LogIt
    @Column(name = "UPDATE_DATE")
    private LocalDateTime updateDate;

    @LogIt
    @Column(name = "SUBDIVISION_ID")
    private Long subdivisionId;

    public AreaMedicalEmployees() {
    }

    public AreaMedicalEmployees(Long medicalEmployeeJobId, Area area, Boolean replacement, LocalDate startDate,
                                LocalDate endDate, String snils, Optional<PositionCode> positionCode,
                                LocalDateTime createDate, LocalDateTime updateDate, Long subdivisionId) {
        this(null, medicalEmployeeJobId, area, replacement, startDate, endDate, snils, positionCode, createDate, updateDate, subdivisionId);
    }

    public AreaMedicalEmployees(Long id, Long medicalEmployeeJobId, Area area, Boolean replacement, LocalDate startDate,
                                LocalDate endDate, String snils, Optional<PositionCode> positionCode,
                                LocalDateTime createDate, LocalDateTime updateDate, Long subdivisionId) {
        this.id = id;
        this.medicalEmployeeJobId = medicalEmployeeJobId;
        this.area = area;
        this.replacement = replacement;
        this.startDate = startDate;
        this.endDate = endDate;
        this.snils = snils;
        this.positionCode = positionCode.map(PositionCode::getCode).orElse(null);
        this.createDate = createDate;
        this.updateDate = updateDate;
        this.subdivisionId = subdivisionId;

    }


    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getMedicalEmployeeJobId() {
        return medicalEmployeeJobId;
    }

    public void setMedicalEmployeeJobId(Long medicalEmployeeJobId) {
        this.medicalEmployeeJobId = medicalEmployeeJobId;
    }

    public Area getArea() {
        return area;
    }

    public void setArea(Area area) {
        this.area = area;
    }

    public Boolean getReplacement() {
        return replacement;
    }

    public void setReplacement(Boolean replacement) {
        this.replacement = replacement;
    }

    public LocalDate getStartDate() {
        return startDate;
    }

    public void setStartDate(LocalDate startDate) {
        this.startDate = startDate;
    }

    public LocalDate getEndDate() {
        return endDate;
    }

    public void setEndDate(LocalDate endDate) {
        this.endDate = endDate;
    }

    public String getSnils() {
        return snils;
    }

    public void setSnils(String snils) {
        this.snils = snils;
    }
    
    public LocalDateTime getCreateDate() {
        return createDate;
    }

    public void setCreateDate(LocalDateTime createDate) {
        this.createDate = createDate;
    }

    public void setUpdateDate(LocalDateTime updateDate) {
        this.updateDate = updateDate;
    }

    public void setSubdivisionId(Long subdivisionId) {
        this.subdivisionId = subdivisionId;
    }

    public String getPositionCode() {
        return positionCode;
    }

    public void setPositionCode(String positionCode) {
        this.positionCode = positionCode;
    }

    public Long getSubdivisionId() {
        return subdivisionId;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj != null && obj instanceof AreaMedicalEmployees) {
            return ((AreaMedicalEmployees) obj).getId().equals(this.id);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(this.id);
    }

    @Override
    public AreaMedicalEmployees clone() {
        try {
            return (AreaMedicalEmployees) super.clone();
        } catch (CloneNotSupportedException e) {
            return null;
        }
    }
}
