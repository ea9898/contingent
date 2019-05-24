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
import javax.persistence.OneToOne;
import javax.persistence.Table;
import java.io.Serializable;
import java.util.Objects;

@Entity
@Table(name = "PR_AREA_TYPE_ATTRS")
@Cacheable
public class PrimaryAreaTypeAttributes implements Serializable {

    private static final long serialVersionUID = 6377158523177395350L;

    @Id
    @Column(name = "ID", unique = true, nullable = false)
    private Long id;

    @JoinColumn(name = "AREA_TYPE_CODE")
    @OneToOne(fetch = FetchType.LAZY)
    private AreaType areaType;

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

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public AreaType getAreaType() {
        return areaType;
    }

    public void setAreaType(AreaType areaType) {
        this.areaType = areaType;
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

    public AreaCountLimit getAreaCountLimit() {
        return areaCountLimit;
    }

    public void setAreaCountLimit(AreaCountLimit areaCountLimit) {
        this.areaCountLimit = areaCountLimit;
    }

    public Integer getResidentsBindRate() {
        return residentsBindRate;
    }

    public void setResidentsBindRate(Integer residentsBindRate) {
        this.residentsBindRate = residentsBindRate;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj != null && obj instanceof PrimaryAreaTypeAttributes) {
            return ((PrimaryAreaTypeAttributes) obj).getId().equals(this.id);
        }
        return false;
    }

    @Override
    public int hashCode() {        
        return Objects.hashCode(this.id);
    }
}
