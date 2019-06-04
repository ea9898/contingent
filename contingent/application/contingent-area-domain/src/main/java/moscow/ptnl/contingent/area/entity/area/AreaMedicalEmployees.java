package moscow.ptnl.contingent.area.entity.area;

import moscow.ptnl.contingent.area.entity.converter.BooleanIntegerConverter;
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

@Entity
@Table(name = "AREA_MEDICAL_EMPLOYEES")
@SequenceGenerator(name = "seq_area_medical_employees", sequenceName = "seq_area_medical_employees", allocationSize=1)
public class AreaMedicalEmployees implements Serializable {

    private static final long serialVersionUID = 4435222693561566689L;

    @Id
    @GeneratedValue(strategy= GenerationType.SEQUENCE, generator="seq_area_medical_employees")
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

    @JoinColumn(name = "POSITION_CODE")
    @ManyToOne(fetch = FetchType.LAZY)
    private PositionNom positionNom;

    @Column(name = "CREATE_DATE", nullable = false)
    private LocalDateTime createDate;

    @Column(name = "UPDATE_DATE")
    private LocalDateTime updateDate;

    @Column(name = "SUBDIVISION_ID")
    private Long subdivisionId;

    public AreaMedicalEmployees() {
    }

    public AreaMedicalEmployees(Long medicalEmployeeJobInfoId, Area area, Boolean replacement, LocalDate startDate,
                                LocalDate endDate, String snils, PositionNom positionNom,
                                LocalDateTime createDate, LocalDateTime updateDate, Long subdivisionId) {
        this(null, medicalEmployeeJobInfoId, area, replacement, startDate, endDate, snils, positionNom, createDate, updateDate, subdivisionId);
    }

    public AreaMedicalEmployees(Long id, Long medicalEmployeeJobInfoId, Area area, Boolean replacement, LocalDate startDate,
                                LocalDate endDate, String snils, PositionNom positionNom,
                                LocalDateTime createDate, LocalDateTime updateDate, Long subdivisionId) {
        this.id = id;
        this.medicalEmployeeJobInfoId = medicalEmployeeJobInfoId;
        this.area = area;
        this.replacement = replacement;
        this.startDate = startDate;
        this.endDate = endDate;
        this.snils = snils;
        this.positionNom = positionNom;
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
