package moscow.ptnl.contingent.area.entity.area;

import moscow.ptnl.contingent.area.entity.nsi.AreaTypes;

import javax.persistence.Cacheable;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.validation.constraints.Size;
import java.io.Serializable;

@Entity
@Table(name = "AREAS")
@Cacheable
public class Area implements Serializable {

    private static final long serialVersionUID = 4575089048144449304L;

    @Id
	@Column(name = "ID", unique = true, nullable = false)
	private Long id;

	@Column(name = "MO_ID")
	private Long moId;

    @JoinColumn(name = "AREA_TYPE_CODE")
    @ManyToOne(fetch = FetchType.LAZY)
    private AreaTypes areaType;

    @Size(max = 255)
    @Column(name = "NAME")
    private String name;

    @Column(name = "NUMBER")
    private Integer number;

    @Column(name = "OMS")
    private Boolean oms;

    @Column(name = "DMS")
    private Boolean dms;

    @Column(name = "PAID")
    private Boolean paid;

    @Column(name = "IS_AUTO_ASSIGN_FOR_ATTACH")
    private Boolean autoAssignForAttach;

    @Column(name = "ACTUAL")
    private Boolean actual;

    @Size(max = 370)
    @Column(name = "DESCRIPTION")
    private String description;

    @Column(name = "ATTACH_BY_MEDICAL_REASON")
    private Boolean attachByMedicalReason;

	@Column(name = "AGE_MIN")
	private Integer ageMin;

	@Column(name = "AGE_MAX")
	private Integer ageMax;

	@Column(name = "AGE_M_MIN")
	private Integer ageMMin;

	@Column(name = "AGE_M_MAX")
	private Integer ageMMax;

	@Column(name = "AGE_W_MIN")
	private Integer ageWMin;

	@Column(name = "AGE_W_MAX")
	private Integer ageWMax;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getMoId() {
        return moId;
    }

    public void setMoId(Long moId) {
        this.moId = moId;
    }

    public AreaTypes getAreaType() {
        return areaType;
    }

    public void setAreaType(AreaTypes areaType) {
        this.areaType = areaType;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Integer getNumber() {
        return number;
    }

    public void setNumber(Integer number) {
        this.number = number;
    }

    public Boolean getOms() {
        return oms;
    }

    public void setOms(Boolean oms) {
        this.oms = oms;
    }

    public Boolean getDms() {
        return dms;
    }

    public void setDms(Boolean dms) {
        this.dms = dms;
    }

    public Boolean getPaid() {
        return paid;
    }

    public void setPaid(Boolean paid) {
        this.paid = paid;
    }

    public Boolean getAutoAssignForAttach() {
        return autoAssignForAttach;
    }

    public void setAutoAssignForAttach(Boolean autoAssignForAttach) {
        this.autoAssignForAttach = autoAssignForAttach;
    }

    public Boolean getActual() {
        return actual;
    }

    public void setActual(Boolean actual) {
        this.actual = actual;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public Boolean getAttachByMedicalReason() {
        return attachByMedicalReason;
    }

    public void setAttachByMedicalReason(Boolean attachByMedicalReason) {
        this.attachByMedicalReason = attachByMedicalReason;
    }

    public Integer getAgeMin() {
        return ageMin;
    }

    public void setAgeMin(Integer ageMin) {
        this.ageMin = ageMin;
    }

    public Integer getAgeMax() {
        return ageMax;
    }

    public void setAgeMax(Integer ageMax) {
        this.ageMax = ageMax;
    }

    public Integer getAgeMMin() {
        return ageMMin;
    }

    public void setAgeMMin(Integer ageMMin) {
        this.ageMMin = ageMMin;
    }

    public Integer getAgeMMax() {
        return ageMMax;
    }

    public void setAgeMMax(Integer ageMMax) {
        this.ageMMax = ageMMax;
    }

    public Integer getAgeWMin() {
        return ageWMin;
    }

    public void setAgeWMin(Integer ageWMin) {
        this.ageWMin = ageWMin;
    }

    public Integer getAgeWMax() {
        return ageWMax;
    }

    public void setAgeWMax(Integer ageWMax) {
        this.ageWMax = ageWMax;
    }
}
