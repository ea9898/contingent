package moscow.ptnl.contingent.area.entity.area;

import moscow.ptnl.contingent.area.entity.converter.BooleanIntegerConverter;
import moscow.ptnl.contingent.area.entity.nsi.PositionNom;

import javax.persistence.Cacheable;
import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.validation.constraints.Size;
import java.io.Serializable;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Objects;

@Entity
@Table(name = "AREA_MEDICAL_EMPLOYEE")
@Cacheable
public class AreaMedicalEmployee implements Serializable {

    private static final long serialVersionUID = 4435222693561566689L;

    @Id
    @Size(max = 50)
    @Column(name = "ID", unique = true, nullable = false)
    private String id;

    @Column(name = "MEDICAL_EMPLOYEE_JOB_INFO_ID")
    private Long medicalEmployeeJobInfoId;

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
    private String snils;

    @Column(name = "MEDICAL_POSITION_ID")
    private Long medicalPositionId;

    @JoinColumn(name = "POSITION_NOM_ID")
    @ManyToOne(fetch = FetchType.LAZY)
    private PositionNom positionNom;

    @Column(name = "CREATE_DATE", nullable = false)
    private LocalDateTime createDate;

    @Column(name = "UPDATE_DATE", nullable = false)
    private LocalDateTime updateDate;

    @Column(name = "deleted", nullable = false)
    private Boolean isDeleted;

    @Column(name = "subdivision_id", nullable = false)
    private Long subdivisionId;

    public AreaMedicalEmployee() {
    }

    public AreaMedicalEmployee(Long medicalEmployeeJobInfoId, Area area, Boolean replacement, LocalDate startDate,
                               LocalDate endDate, String snils, Long medicalPositionId, LocalDateTime createDate,
                               LocalDateTime updateDate, Boolean isDeleted, Long subdivisionId) {
        this.medicalEmployeeJobInfoId = medicalEmployeeJobInfoId;
        this.area = area;
        this.replacement = replacement;
        this.startDate = startDate;
        this.endDate = endDate;
        this.snils = snils;
        this.medicalPositionId = medicalPositionId;
        this.createDate = createDate;
        this.updateDate = updateDate;
        this.isDeleted = isDeleted;
        this.subdivisionId = subdivisionId;
    }

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public Long getMedicalEmployeeJobInfoId() {
        return medicalEmployeeJobInfoId;
    }

    public void setMedicalEmployeeJobInfoId(Long medicalEmployeeJobInfoId) {
        this.medicalEmployeeJobInfoId = medicalEmployeeJobInfoId;
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

    public Long getMedicalPositionId() {
        return medicalPositionId;
    }

    public void setMedicalPositionId(Long medicalPositionId) {
        this.medicalPositionId = medicalPositionId;
    }

    public PositionNom getPositionNom() {
        return positionNom;
    }

    public void setPositionNom(PositionNom positionNom) {
        this.positionNom = positionNom;
    }

    public LocalDateTime getCreateDate() {
        return createDate;
    }

    public void setCreateDate(LocalDateTime createDate) {
        this.createDate = createDate;
    }

    public LocalDateTime getUpdateDate() {
        return updateDate;
    }

    public void setUpdateDate(LocalDateTime updateDate) {
        this.updateDate = updateDate;
    }

    public Boolean isDeleted() {
        return isDeleted;
    }

    public void setDeleted(Boolean deleted) {
        isDeleted = deleted;
    }

    public Long getSubdivisionId() {
        return subdivisionId;
    }

    public void setSubdivisionId(Long subdivisionId) {
        this.subdivisionId = subdivisionId;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj != null && obj instanceof AreaMedicalEmployee) {
            return ((AreaMedicalEmployee) obj).getId().equals(this.id);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(this.id);
    }
}
