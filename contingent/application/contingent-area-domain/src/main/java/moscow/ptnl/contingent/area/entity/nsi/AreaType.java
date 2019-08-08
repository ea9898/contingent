package moscow.ptnl.contingent.area.entity.nsi;

import moscow.ptnl.contingent.area.entity.converter.BooleanIntegerConverter;
import moscow.ptnl.contingent.area.entity.converter.BooleanStrictIntegerConverter;
import org.hibernate.annotations.Proxy;

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
import java.util.Objects;

@Entity
@Table(name = "AREA_TYPE")
@Cacheable
@Proxy(lazy=false)
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
    private AreaTypeKind areaTypeKind;

    @JoinColumn(name = "AREA_TYPE_CLASS_CODE")
    @ManyToOne(fetch = FetchType.LAZY)
    private AreaTypeClass areaTypeClass;

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

    @Column(name = "AREA_COUNT_LIMIT_CODE")
    private Integer areaCountLimit;

    @Column(name = "RESIDENTS_BIND_RATE")
    private Integer residentsBindRate;

    public AreaType() {
    }

    public AreaType(Long code, String title, Boolean archive) {
        this.code = code;
        this.title = title;
        this.archived = archive;
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

    public AreaTypeKind getAreaTypeKind() {
        return areaTypeKind;
    }

    public void setAreaTypeKind(AreaTypeKind areaTypeKind) {
        this.areaTypeKind = areaTypeKind;
    }

    public AreaTypeClass getAreaTypeClass() {
        return areaTypeClass;
    }

    public void setAreaTypeClass(AreaTypeClass areaTypeClass) {
        this.areaTypeClass = areaTypeClass;
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

    public Integer getResidentsBindRate() {
        return residentsBindRate;
    }

    public void setResidentsBindRate(Integer residentsBindRate) {
        this.residentsBindRate = residentsBindRate;
    }

    public Boolean getHeadFinance() {
        return headFinance;
    }

    public void setHeadFinance(Boolean headFinance) {
        this.headFinance = headFinance;
    }

    public Boolean getHasServiceTerritory() {
        return hasServiceTerritory;
    }

    public void setHasServiceTerritory(Boolean hasServiceTerritory) {
        this.hasServiceTerritory = hasServiceTerritory;
    }

    public Boolean getAttachByRequest() {
        return attachByRequest;
    }

    public void setAttachByRequest(Boolean attachByRequest) {
        this.attachByRequest = attachByRequest;
    }

    public Boolean getAttachByMedicalReason() {
        return attachByMedicalReason;
    }

    public void setAttachByMedicalReason(Boolean attachByMedicalReason) {
        this.attachByMedicalReason = attachByMedicalReason;
    }

    public Boolean getMpguAvailable() {
        return mpguAvailable;
    }

    public void setMpguAvailable(Boolean mpguAvailable) {
        this.mpguAvailable = mpguAvailable;
    }

    public Integer getAreaCountLimit() {
        return areaCountLimit;
    }

    public void setAreaCountLimit(Integer areaCountLimit) {
        this.areaCountLimit = areaCountLimit;
    }

    @Override
    public int hashCode() {        
        return Objects.hashCode(this.code);
    }
    
    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj instanceof AreaType) {
            return ((AreaType) obj).getCode().equals(this.code);
        }
        return false;
    }

    public enum FieldsEnum {
        CODE,
        TITLE,
        AREA_TYPE_KIND_CODE,
        AREA_TYPE_CLASS_CODE,
        GENDER_CODE,
        AGE_MIN,
        AGE_MAX,
        AGE_M_MIN,
        AGE_M_MAX,
        AGE_W_MIN,
        AGE_W_MAX,
        HEAD_FINANCE,
        HAS_SERVICE_TERRITORY,
        ATTACH_BY_REQUEST,
        ATTACH_BY_MEDICAL_REASON,
        MPGU_AVAILABLE,
        RESIDENTS_BIND_RATE,
        AREA_COUNT_LIMIT_CODE,
        ARCHIVED
    }
}
