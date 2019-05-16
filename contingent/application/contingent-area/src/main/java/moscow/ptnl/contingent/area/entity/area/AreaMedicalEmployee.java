package moscow.ptnl.contingent.area.entity.area;

import moscow.ptnl.contingent.area.entity.converter.BooleanIntegerConverter;
import moscow.ptnl.contingent.area.entity.converter.BooleanStrictIntegerConverter;
import moscow.ptnl.contingent.area.entity.nsi.PositionNomClinic;

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
public class AreaMedicalEmployee implements Serializable {

    private static final long serialVersionUID = 4435222693561566689L;

    @Id
    @Column(name = "ID", unique = true, nullable = false)
    private Long id;

    @Column(name = "MEDICAL_EMPLOYEE_JOB_INFO_ID")
    private Long medicalEmployeeJobInfoId;

    @JoinColumn(name = "AREA_ID")
    @ManyToOne(fetch = FetchType.LAZY)
    private Area area;

    @Column(name = "IS_REPLACEMENT")
    @Convert(converter = BooleanIntegerConverter.class)
    private Boolean replacement;

    @Column(name = "OUZ")
    @Size(max = 50)
    private String ouz;

    @Column(name = "START_DATE")
    private LocalDate startDate;

    @Column(name = "END_DATE")
    private LocalDate endDate;

    @Column(name = "SNILS")
    @Size(max = 20)
    private String snils;

    @Column(name = "MEDICAL_POSITION_ID")
    private Long medicalPositionId;

    @JoinColumn(name = "POSITION_NOM_CLINIC_ID")
    @ManyToOne(fetch = FetchType.LAZY)
    private PositionNomClinic positionNomClinic;

    @Column(name = "CREATE_DATE", nullable = false)
    private LocalDateTime createDate;

    @Column(name = "UPDATE_DATE")
    private LocalDateTime updateDate;

    @Column(name = "SUBDIVISION_ID")
    private Long subdivisionId;

    @Column(name = "ARCHIVED")
    @Convert(converter = BooleanStrictIntegerConverter.class)
    private Boolean archived;

    public AreaMedicalEmployee() {
    }

    public AreaMedicalEmployee(Long medicalEmployeeJobInfoId, Area area, Boolean replacement, LocalDate startDate,
                               LocalDate endDate, String snils, Long medicalPositionId, LocalDateTime createDate,
                               LocalDateTime updateDate, Boolean archived, Long subdivisionId) {
        this.medicalEmployeeJobInfoId = medicalEmployeeJobInfoId;
        this.area = area;
        this.replacement = replacement;
        this.startDate = startDate;
        this.endDate = endDate;
        this.snils = snils;
        this.medicalPositionId = medicalPositionId;
        this.createDate = createDate;
        this.updateDate = updateDate;
        this.archived = archived;
        this.subdivisionId = subdivisionId;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
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

    public PositionNomClinic getPositionNomClinic() {
        return positionNomClinic;
    }

    public void setPositionNomClinic(PositionNomClinic positionNomClinic) {
        this.positionNomClinic = positionNomClinic;
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

    public Long getSubdivisionId() {
        return subdivisionId;
    }

    public void setSubdivisionId(Long subdivisionId) {
        this.subdivisionId = subdivisionId;
    }

    public String getOuz() {
        return ouz;
    }

    public void setOuz(String ouz) {
        this.ouz = ouz;
    }

    public Boolean getArchived() {
        return archived;
    }

    public void setArchived(Boolean archived) {
        this.archived = archived;
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
