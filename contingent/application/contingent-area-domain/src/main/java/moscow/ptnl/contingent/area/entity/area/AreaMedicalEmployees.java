package moscow.ptnl.contingent.area.entity.area;

import moscow.ptnl.contingent.area.entity.converter.BooleanIntegerConverter;
import moscow.ptnl.contingent.area.entity.nsi.PositionCode;
import moscow.ptnl.contingent.area.entity.nsi.PositionNom;

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

@Entity
@Table(name = "AREA_MEDICAL_EMPLOYEES")
@SequenceGenerator(name = "seq_area_medical_employee", sequenceName = "seq_area_medical_employee", allocationSize=1)
public class AreaMedicalEmployees implements Serializable {

    private static final long serialVersionUID = 4435222693561566689L;

    @Id
    @GeneratedValue(strategy= GenerationType.SEQUENCE, generator="seq_area_medical_employee")
    @Column(name = "ID", unique = true, nullable = false)
    private Long id;

    @Column(name = "MEDICAL_EMPLOYEE_JOB_ID")
    private Long medicalEmployeeJobId;

    @JoinColumn(name = "AREA_ID")
    @ManyToOne(fetch = FetchType.LAZY)
    private Area area;

    @Column(name = "IS_REPLACEMENT")
    @Convert(converter = BooleanIntegerConverter.class)
    private Boolean replacement;

    @Column(name = "START_DATE")
    private LocalDate startDate;

    @Column(name = "END_DATE")
    private LocalDate endDate;

    @Column(name = "SNILS")
    @Size(max = 20)
    private String snils;
    
    @Column(name = "POSITION_CODE")
    private String positionCode;
    
    @Column(name = "CREATE_DATE", nullable = false)
    private LocalDateTime createDate;

    @Column(name = "UPDATE_DATE")
    private LocalDateTime updateDate;

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
}
