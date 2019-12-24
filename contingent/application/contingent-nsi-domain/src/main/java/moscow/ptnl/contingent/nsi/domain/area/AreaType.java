package moscow.ptnl.contingent.nsi.domain.area;

import moscow.ptnl.contingent.nsi.domain.NsiExternalEntity;
import org.hibernate.annotations.Proxy;

import javax.persistence.Cacheable;
import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.validation.constraints.Size;
import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.Objects;
import java.util.Set;

import moscow.ptnl.contingent.domain.Keyable;
import moscow.ptnl.contingent.domain.converter.BooleanIntegerConverter;
import moscow.ptnl.contingent.domain.converter.BooleanStrictIntegerConverter;
import moscow.ptnl.contingent.nsi.domain.NsiTablesEnum;
import moscow.ptnl.contingent.nsi.domain.annotation.MapToNsi;


@Entity
@Table(name = "AREA_TYPE")
@Cacheable
@Proxy(lazy=false)
@MapToNsi(table = NsiTablesEnum.AREA_TYPE)
public class AreaType extends CodeName implements Serializable, Keyable, NsiExternalEntity {

    private static final long serialVersionUID = -1047920239396677745L;

    @Id
    @Column(name = "CODE", unique = true, nullable = false)
    @MapToNsi
    private Long code;

    @Size(max = 100)
    @Column(name = "TITLE")
    @MapToNsi
    private String title;

    @JoinColumn(name = "AREA_TYPE_KIND_CODE")
    @ManyToOne(fetch = FetchType.LAZY)
    @MapToNsi(value = "AREA_TYPE_KIND_CODE", findEntityByField = "globalId")
    private AreaTypeKind areaTypeKind;

    @JoinColumn(name = "AREA_TYPE_CLASS_CODE")
    @ManyToOne(fetch = FetchType.LAZY)
    @MapToNsi(value = "AREA_TYPE_CLASS_CODE", findEntityByField = "globalId")
    private AreaTypeClass areaTypeClass;

    @Size(max = 50)
    @Column(name = "GENDER_CODE")
    @MapToNsi(value = "GENDER_CODE", findEntityByField = "code", crossObject = Gender.class, crossField = "globalId")
    private String gender;

    @Column(name = "AGE_MIN")
    @MapToNsi("AGE_MIN")
    private Integer ageMin;

    @Column(name = "AGE_MAX")
    @MapToNsi("AGE_MAX")
    private Integer ageMax;

    @Column(name = "AGE_M_MIN")
    @MapToNsi("AGE_M_MIN")
    private Integer ageMMin;

    @Column(name = "AGE_M_MAX")
    @MapToNsi("AGE_M_MAX")
    private Integer ageMMax;

    @Column(name = "AGE_W_MIN")
    @MapToNsi("AGE_W_MIN")
    private Integer ageWMin;

    @Column(name = "AGE_W_MAX")
    @MapToNsi("AGE_W_MAX")
    private Integer ageWMax;

    @Column(name = "ARCHIVED")
    @Convert(converter = BooleanStrictIntegerConverter.class)
    @MapToNsi
    private Boolean archived;

    @Column(name = "HEAD_FINANCE")
    @Convert(converter = BooleanIntegerConverter.class)
    @MapToNsi("HEAD_FINANCE")
    private Boolean headFinance;

    @Column(name = "HAS_SERVICE_TERRITORY")
    @Convert(converter = BooleanIntegerConverter.class)
    @MapToNsi("HAS_SERVICE_TERRITORY")
    private Boolean hasServiceTerritory;

    @Column(name = "ATTACH_BY_REQUEST")
    @Convert(converter = BooleanIntegerConverter.class)
    @MapToNsi("ATTACH_BY_REQUEST")
    private Boolean attachByRequest;

    @Column(name = "ATTACH_BY_MEDICAL_REASON")
    @Convert(converter = BooleanIntegerConverter.class)
    @MapToNsi("ATTACH_BY_MEDICAL_REASON")
    private Boolean attachByMedicalReason;

    @Column(name = "MPGU_AVAILABLE")
    @Convert(converter = BooleanIntegerConverter.class)
    @MapToNsi("MPGU_AVAILABLE")
    private Boolean mpguAvailable;

    @Column(name = "AREA_COUNT_LIMIT_CODE")
    @MapToNsi("AREA_COUNT_LIMIT_CODE")
    private Integer areaCountLimit;

    @Column(name = "RESIDENTS_BIND_RATE")
    @MapToNsi("RESIDENTS_BIND_RATE")
    private Integer residentsBindRate;

    @Column(name = "GLOBAL_ID")
    @MapToNsi("GLOBAL_ID")
    private Long globalId;

    @Column(name = "UPDATE_DATE", nullable = false)
    private LocalDateTime updateDate;

    @Column(name = "SOURCE")
    @Size(max = 4000)
    private String source;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "areaType")
    private Set<AreaTypeSpecializations> areaTypeSpecializations;

    public AreaType() {
    }

    public AreaType(Long code) {
        this.code = code;
    }

    public AreaType(Long code, String title, Boolean archive) {
        this.code = code;
        this.title = title;
        this.archived = archive;
    }

    @Override
    public Long getCode() {
        return code;
    }

    @Override
    public void setCode(Long code) {
        this.code = code;
    }

    public Long getGlobalId() {
        return globalId;
    }

    public void setGlobalId(Long globalId) {
        this.globalId = globalId;
    }

    @Override
    public String getTitle() {
        return title;
    }

    @Override
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
    public LocalDateTime getUpdateDate() {
        return updateDate;
    }

    @Override
    public void setUpdateDate(LocalDateTime updateDate) {
        this.updateDate = updateDate;
    }

    @Override
    public String getSource() {
        return source;
    }

    @Override
    public void setSource(String source) {
        this.source = source;
    }

    public Set<AreaTypeSpecializations> getAreaTypeSpecializations() {
        return areaTypeSpecializations;
    }

    public void setAreaTypeSpecializations(Set<AreaTypeSpecializations> areaTypeSpecializations) {
        this.areaTypeSpecializations = areaTypeSpecializations;
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

    @Override
    public Serializable getKey() {
        return getCode();
    }
}
