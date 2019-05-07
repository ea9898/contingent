package moscow.ptnl.contingent.area.entity.area;

import moscow.ptnl.contingent.area.entity.converter.BooleanIntegerConverter;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypes;

import javax.persistence.Cacheable;
import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.validation.constraints.Size;
import java.io.Serializable;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

@Entity
@Table(name = "AREAS")
@SequenceGenerator(name = "SEQ_AREAS", sequenceName = "SEQ_AREAS", allocationSize=1)
@Cacheable
public class Area implements Serializable {

    private static final long serialVersionUID = 4575089048144449304L;

    @Id
    @GeneratedValue(strategy= GenerationType.SEQUENCE, generator="SEQ_AREAS")
	@Column(name = "ID", unique = true, nullable = false)
	private Long id;

	@Column(name = "MO_ID")
	private Long moId;

	@Column(name = "MU_ID", nullable = false)
	private Long muId;

    @JoinColumn(name = "AREA_TYPE_CODE")
    @ManyToOne(fetch = FetchType.LAZY)
    private AreaTypes areaType;

    @Column(name = "NUMBER")
    private Integer number;

    @Column(name = "IS_AUTO_ASSIGN_FOR_ATTACH")
    @Convert(converter = BooleanIntegerConverter.class)
    private Boolean autoAssignForAttach;

    @Column(name = "ARCHIVED")
    @Convert(converter = BooleanIntegerConverter.class)
    private Boolean archived;

    @Size(max = 370)
    @Column(name = "DESCRIPTION")
    private String description;

    @Column(name = "ATTACH_BY_MEDICAL_REASON")
    @Convert(converter = BooleanIntegerConverter.class)
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

    @Column(name = "CREATE_DATE", nullable = false)
    private LocalDateTime createDate;

    @Column(name = "UPDATE_DATE", nullable = false)
    private LocalDateTime updateDate;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "area")
    private Set<AreaMedicalEmployee> medicalEmployees;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "area")
    private Set<AreaToAreaType> primaryAreaTypes;

    public Area() {
    }

    public Area(Long moId, Long muId, AreaTypes areaType, Integer number, Boolean autoAssignForAttach, Boolean archived, String description, Boolean attachByMedicalReason, Integer ageMin, Integer ageMax, Integer ageMMin, Integer ageMMax, Integer ageWMin, Integer ageWMax, LocalDateTime createDate) {
        this.moId = moId;
        this.muId = muId;
        this.areaType = areaType;
        this.number = number;
        this.autoAssignForAttach = autoAssignForAttach;
        this.archived = archived;
        this.description = description;
        this.attachByMedicalReason = attachByMedicalReason;
        this.ageMin = ageMin;
        this.ageMax = ageMax;
        this.ageMMin = ageMMin;
        this.ageMMax = ageMMax;
        this.ageWMin = ageWMin;
        this.ageWMax = ageWMax;
        this.createDate = createDate;
        this.updateDate = getCreateDate();
    }

    public Area(Area copy) {
        setMoId(copy.getMoId());
        setMuId(copy.getMuId());
        setAreaType(copy.getAreaType());
        setArchived(copy.getArchived());
        setCreateDate(copy.getCreateDate());
        setUpdateDate(copy.getUpdateDate());
        setAutoAssignForAttach(copy.getAutoAssignForAttach());
        setDescription(copy.getDescription());
        setAttachByMedicalReason(copy.getAttachByMedicalReason());
        setAgeMin(copy.getAgeMin());
        setAgeMax(copy.getAgeMax());
        setAgeMMin(copy.getAgeMMin());
        setAgeMMax(copy.getAgeMMax());
        setAgeWMin(copy.getAgeWMin());
        setAgeWMax(copy.getAgeWMax());
        setNumber(copy.getNumber());
    }

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

    public Long getMuId() {
        return muId;
    }

    public void setMuId(Long muId) {
        this.muId = muId;
    }

    public AreaTypes getAreaType() {
        return areaType;
    }

    public void setAreaType(AreaTypes areaType) {
        this.areaType = areaType;
    }

    public Integer getNumber() {
        return number;
    }

    public void setNumber(Integer number) {
        this.number = number;
    }

    public Boolean getAutoAssignForAttach() {
        return autoAssignForAttach;
    }

    public void setAutoAssignForAttach(Boolean autoAssignForAttach) {
        this.autoAssignForAttach = autoAssignForAttach;
    }

    public Boolean getArchived() {
        return archived;
    }

    public void setArchived(Boolean archived) {
        this.archived = archived;
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

    public Set<AreaMedicalEmployee> getMedicalEmployees() {
        return medicalEmployees;
    }

    public Set<AreaMedicalEmployee> getActualMedicalEmployees() {
        return medicalEmployees.stream()
                .filter(e -> e.getEndDate() == null || e.getEndDate().isAfter(LocalDate.now()))
                .collect(Collectors.toSet());
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

    public Set<AreaToAreaType> getPrimaryAreaTypes() {
        return primaryAreaTypes;
    }
}
