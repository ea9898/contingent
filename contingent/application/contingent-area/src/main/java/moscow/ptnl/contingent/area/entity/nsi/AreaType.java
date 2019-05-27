package moscow.ptnl.contingent.area.entity.nsi;

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
    @Column(name = "NAME")
    private String name;

    @JoinColumn(name = "AREA_TYPE_KIND_CODE")
    @ManyToOne(fetch = FetchType.LAZY)
    private AreaTypesKind kindAreaType;

    @JoinColumn(name = "AREA_TYPE_CLASS_CODE")
    @ManyToOne(fetch = FetchType.LAZY)
    private AreaTypesClass classAreaType;

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

    @Column(name = "ARCHIVED")
    @Convert(converter = BooleanStrictIntegerConverter.class)
    private Boolean archived;

    @JoinColumn(name = "SPECIALIZATION_ID")
    @ManyToOne(fetch = FetchType.LAZY)
    private Specialization specialization;

    @OneToOne(fetch = FetchType.LAZY, mappedBy = "areaType")
    private PrimaryAreaTypeAttributes attributes;

    public AreaType() {
    }

    public AreaType(Long code, String name, Boolean archived, Specialization specialization) {
        this.code = code;
        this.name = name;
        this.archived = archived;
        this.specialization = specialization;
    }

    public Long getCode() {
        return code;
    }

    public void setCode(Long code) {
        this.code = code;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public AreaTypesKind getKindAreaType() {
        return kindAreaType;
    }

    public void setKindAreaType(AreaTypesKind kindAreaType) {
        this.kindAreaType = kindAreaType;
    }

    public AreaTypesClass getClassAreaType() {
        return classAreaType;
    }

    public void setClassAreaType(AreaTypesClass classAreaType) {
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

    public PrimaryAreaTypeAttributes getAttributes() {
        return attributes;
    }

    public void setAttributes(PrimaryAreaTypeAttributes attributes) {
        this.attributes = attributes;
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
