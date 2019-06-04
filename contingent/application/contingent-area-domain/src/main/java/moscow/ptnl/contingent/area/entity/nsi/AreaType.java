package moscow.ptnl.contingent.area.entity.nsi;

import moscow.ptnl.contingent.area.entity.converter.BooleanIntegerConverter;
import moscow.ptnl.contingent.area.entity.converter.BooleanStrictIntegerConverter;

import javax.persistence.Cacheable;
import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.validation.constraints.Size;
import java.io.Serializable;
import java.util.Objects;

@Entity
@Table(name = "AREA_TYPE")
@Cacheable
public class AreaType implements Serializable {

    private static final long serialVersionUID = -1047920239396677745L;

    @Id
    @Column(name = "CODE", unique = true, nullable = false)
    private Long code;

    @Size(max = 100)
    @Column(name = "TITLE")
    private String title;

    @JoinColumn(name = "AREA_TYPE_KIND_CODE")
    @ManyToOne(fetch = FetchType.LAZY)
    private AreaTypeKind kindAreaType;

    @JoinColumn(name = "AREA_TYPE_CLASS_CODE")
    @ManyToOne(fetch = FetchType.LAZY)
    private AreaTypeClass classAreaType;

    @Size(max = 1)
    @Column(name = "GENDER_CODE")
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

    @Column(name = "ARCHIVED")
    @Convert(converter = BooleanStrictIntegerConverter.class)
    private Boolean archived;

    @Column(name = "HEAD_FINANCE")
    @Convert(converter = BooleanIntegerConverter.class)
    private Boolean headFinance;

    @Column(name = "HAS_SERVICE_TERRITORY")
    @Convert(converter = BooleanIntegerConverter.class)
    private Boolean hasServiceTerritory;

    @Column(name = "ATTACH_BY_REQUEST")
    @Convert(converter = BooleanIntegerConverter.class)
    private Boolean attachByRequest;

    @Column(name = "ATTACH_BY_MEDICAL_REASON")
    @Convert(converter = BooleanIntegerConverter.class)
    private Boolean attachByMedicalReason;

    @Column(name = "MPGU_AVAILABLE")
    @Convert(converter = BooleanIntegerConverter.class)
    private Boolean mpguAvailable;

    @JoinColumn(name = "AREA_COUNT_LIMIT_CODE")
    @ManyToOne(fetch = FetchType.LAZY)
    private AreaCountLimit areaCountLimit;

    @Column(name = "RESIDENTS_BIND_RATE")
    private Integer residentsBindRate;

    @JoinColumn(name = "SPECIALIZATION_CODE")
    @ManyToOne(fetch = FetchType.LAZY)
    private Specialization specialization;

    public AreaType() {
    }

    public AreaType(Long code, String title, Boolean archive, Specialization specialization) {
        this.code = code;
        this.title = title;
        this.archived = archive;
        this.specialization = specialization;
    }

    public Long getCode() {
        return code;
    }

    public void setCode(Long code) {
        this.code = code;
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public AreaTypeKind getKindAreaType() {
        return kindAreaType;
    }

    public void setKindAreaType(AreaTypeKind kindAreaType) {
        this.kindAreaType = kindAreaType;
    }

    public AreaTypeClass getClassAreaType() {
        return classAreaType;
    }

    public void setClassAreaType(AreaTypeClass classAreaType) {
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

    public Boolean getArchived() { return archived; }

    public void setArchived(Boolean archived) { this.archived = archived; }

    public Specialization getSpecialization() {
        return specialization;
    }

    public void setSpecialization(Specialization specialization) {
        this.specialization = specialization;
    }

    @Override
    public int hashCode() {        
        return Objects.hashCode(this.code);
    }
    
    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj != null && obj instanceof AreaType) {
            return ((AreaType) obj).getCode().equals(this.code);
        }
        return false;
    }
}
