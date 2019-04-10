package moscow.ptnl.contingent.area.entity.nsi;

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

@Entity
@Table(name = "AREA_TYPES")
@Cacheable
public class AreaTypes implements Serializable {

    private static final long serialVersionUID = -1047920239396677745L;

    @Id
    @Size(max = 370)
	@Column(name = "CODE", unique = true, nullable = false)
	private String code;

    @Size(max = 100)
	@Column(name = "NAME")
	private String name;

    @JoinColumn(name = "KIND_AREA_TYPE_CODE")
    @ManyToOne(fetch = FetchType.LAZY)
	private KindAreaTypes kindAreaType;

    @JoinColumn(name = "CLASS_AREA_TYPE_CODE")
    @ManyToOne(fetch = FetchType.LAZY)
	private ClassAreaTypes classAreaType;

    @Size(max = 1)
	@Column(name = "GENDER")
	private String gender;

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

	@Column(name = "ACTUAL")
    @Convert(converter = BooleanIntegerConverter.class)
	private Boolean actual;

    @JoinColumn(name = "SPECIALIZATION_ID")
    @ManyToOne(fetch = FetchType.LAZY)
	private Specialization specialization;

    public String getCode() {
        return code;
    }

    public void setCode(String code) {
        this.code = code;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public KindAreaTypes getKindAreaType() {
        return kindAreaType;
    }

    public void setKindAreaType(KindAreaTypes kindAreaType) {
        this.kindAreaType = kindAreaType;
    }

    public ClassAreaTypes getClassAreaType() {
        return classAreaType;
    }

    public void setClassAreaType(ClassAreaTypes classAreaType) {
        this.classAreaType = classAreaType;
    }

    public String getGender() {
        return gender;
    }

    public void setGender(String gender) {
        this.gender = gender;
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

    public Boolean getActual() {
        return actual;
    }

    public void setActual(Boolean actual) {
        this.actual = actual;
    }

    public Specialization getSpecialization() {
        return specialization;
    }

    public void setSpecialization(Specialization specialization) {
        this.specialization = specialization;
    }
}
