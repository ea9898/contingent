package moscow.ptnl.contingent.area.entity.area;

import moscow.ptnl.contingent.area.entity.converter.BooleanIntegerConverter;

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

    @Column(name = "MEDICAL_EMPLOYEE_ID")
    private Long medicalEmployeeId;

    @Column(name = "MEDICAL_POSITION_ID")
    private Long medicalPositionId;

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

    public Long getMedicalEmployeeId() {
        return medicalEmployeeId;
    }

    public void setMedicalEmployeeId(Long medicalEmployeeId) {
        this.medicalEmployeeId = medicalEmployeeId;
    }

    public Long getMedicalPositionId() {
        return medicalPositionId;
    }

    public void setMedicalPositionId(Long medicalPositionId) {
        this.medicalPositionId = medicalPositionId;
    }
}
