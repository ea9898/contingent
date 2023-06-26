package moscow.ptnl.contingent.domain.area.entity;

import moscow.ptnl.contingent.domain.converter.BooleanIntegerConverter;
import moscow.ptnl.contingent.domain.history.ServiceName;
import moscow.ptnl.contingent.domain.history.meta.Journalable;
import moscow.ptnl.contingent.domain.history.meta.LogIt;
import org.hibernate.annotations.Proxy;

import jakarta.persistence.Column;
import jakarta.persistence.Convert;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.SequenceGenerator;
import jakarta.persistence.Table;
import jakarta.validation.constraints.Size;
import java.io.Serializable;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Objects;
import moscow.ptnl.contingent.domain.history.meta.LogTrigger;

@Entity @Journalable(ServiceName.AREA)
@Proxy(lazy=false)
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
    @Column(name = "IS_REPLACEMENT", nullable = false)
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
    @Column(name = "POSITION_CODE_SUPP")
    private Long positionCodeSupp;

    @LogIt
    @Column(name = "TEMP_DUTY_START_DATE")
    private LocalDate tempDutyStartDate;

    @LogIt
    @Column(name = "CREATE_DATE", nullable = false)
    private LocalDateTime createDate;

    @LogIt
    @Column(name = "UPDATE_DATE")
    private LocalDateTime updateDate;

    @LogIt
    @Column(name = "SUBDIVISION_ID")
    private Long subdivisionId;

    @LogIt
    @Column(name = "IS_ERROR")
    @Convert(converter = BooleanIntegerConverter.class)
    private Boolean isError;

    @LogIt
    @Column(name = "EMPLOYEE_CATEGORY", nullable = false)
    private int employeeCategory;

    public AreaMedicalEmployees() {
    }

    public AreaMedicalEmployees(Long medicalEmployeeJobId, Area area, Boolean replacement, LocalDate startDate,
                                LocalDate endDate, String snils, String positionCode, Long positionCodeSupp,
                                LocalDateTime createDate, LocalDateTime updateDate, Long subdivisionId) {
        this(null, medicalEmployeeJobId, area, replacement, startDate, endDate, snils, positionCode,
                positionCodeSupp, createDate, updateDate, subdivisionId);
    }

    public AreaMedicalEmployees(Long id, Long medicalEmployeeJobId, Area area, Boolean replacement, LocalDate startDate,
                                LocalDate endDate, String snils, String positionCode, Long positionCodeSupp,
                                LocalDateTime createDate, LocalDateTime updateDate, Long subdivisionId) {
        this.id = id;
        this.medicalEmployeeJobId = medicalEmployeeJobId;
        this.area = area;
        this.replacement = replacement;
        this.startDate = startDate;
        this.endDate = endDate;
        this.snils = snils;
        this.positionCode = positionCode;
        this.positionCodeSupp = positionCodeSupp;
        this.createDate = createDate;
        this.updateDate = updateDate;
        this.subdivisionId = subdivisionId;
        this.isError = false;
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

    public LocalDate getTempDutyStartDate() {
        return tempDutyStartDate;
    }

    public void setTempDutyStartDate(LocalDate tempDutyStartDate) {
        this.tempDutyStartDate = tempDutyStartDate;
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

    public Long getPositionCodeSupp() { return positionCodeSupp; }

    public void setPositionCodeSupp(Long positionCodeSupp) { this.positionCodeSupp = positionCodeSupp; }

    public Long getSubdivisionId() {
        return subdivisionId;
    }

    public Boolean getError() { return isError; }

    public void setError(Boolean error) { isError = error; }

    public int getEmployeeCategory() { return employeeCategory; }

    public void setEmployeeCategory(int employeeCategory) { this.employeeCategory = employeeCategory; }


    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj != null && obj instanceof AreaMedicalEmployees) {
            return Objects.equals(((AreaMedicalEmployees) obj).getId(), this.id);
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
