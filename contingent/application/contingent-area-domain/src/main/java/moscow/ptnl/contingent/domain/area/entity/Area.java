package moscow.ptnl.contingent.domain.area.entity;

import moscow.ptnl.contingent.domain.converter.BooleanIntegerConverter;
import moscow.ptnl.contingent.domain.converter.BooleanStrictIntegerConverter;
import moscow.ptnl.contingent.domain.history.converter.AreaTypeFieldConverter;
import moscow.ptnl.contingent.nsi.domain.area.AreaCloseReasons;
import moscow.ptnl.contingent.nsi.domain.area.AreaType;
import moscow.ptnl.contingent.domain.history.ServiceName;
import moscow.ptnl.contingent.domain.history.meta.Journalable;
import moscow.ptnl.contingent.domain.history.meta.LogIt;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeProfile;
import org.hibernate.annotations.Proxy;

import jakarta.persistence.Column;
import jakarta.persistence.Convert;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToMany;
import jakarta.persistence.SequenceGenerator;
import jakarta.persistence.Table;
import jakarta.validation.constraints.Size;
import java.io.Serializable;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

@Entity @Journalable(ServiceName.AREA)
@Proxy(lazy=false)
@Table(name = "AREAS")
@SequenceGenerator(name = "SEQ_AREAS", sequenceName = "SEQ_AREAS", allocationSize=1)
public class Area implements Serializable {

    private static final long serialVersionUID = 4575089048144449304L;

    @Id
    @GeneratedValue(strategy= GenerationType.SEQUENCE, generator="SEQ_AREAS")
    @Column(name = "ID", unique = true, nullable = false)
    private Long id;

    @LogIt
    @Column(name = "MO_ID", nullable = false)
    private Long moId;

    @LogIt
    @Column(name = "MU_ID")
    private Long muId;

    @LogIt(converter = AreaTypeFieldConverter.class)
    @JoinColumn(name = "AREA_TYPE_CODE")
    @ManyToOne(fetch = FetchType.LAZY)
    private AreaType areaType;

    @LogIt
    @Column(name = "NUMBER")
    private Integer number;

    @LogIt
    @Column(name = "SPECIAL_NUMBER")
    private String specialNumber;

    @LogIt
    @Column(name = "IS_AUTO_ASSIGN_FOR_ATTACH")
    @Convert(converter = BooleanIntegerConverter.class)
    private Boolean autoAssignForAttach;

    @LogIt
    @Column(name = "ATT_FINAL_LIMIT")
    private Integer attFinalLimit;

    @LogIt
    @Column(name = "ATT_INFO_LIMIT")
    private Integer attInfoLimit;

    @LogIt
    @Column(name = "ARCHIVED")
    @Convert(converter = BooleanStrictIntegerConverter.class)
    private Boolean archived;

    @LogIt
    @Size(max = 370)
    @Column(name = "DESCRIPTION")
    private String description;

    @LogIt
    @Column(name = "ATTACH_BY_MEDICAL_REASON")
    @Convert(converter = BooleanIntegerConverter.class)
    private Boolean attachByMedicalReason;

    @LogIt
    @JoinColumn(name = "AREA_TYPE_PROFILE_CODE")
    @ManyToOne(fetch = FetchType.LAZY)
    private AreaTypeProfile areaTypeProfile;

    @LogIt
    @Column(name = "AGE_MIN")
    private Integer ageMin;

    @LogIt
    @Column(name = "AGE_MAX")
    private Integer ageMax;

    @LogIt
    @Column(name = "AGE_M_MIN")
    private Integer ageMMin;

    @LogIt
    @Column(name = "AGE_M_MAX")
    private Integer ageMMax;

    @LogIt
    @Column(name = "AGE_W_MIN")
    private Integer ageWMin;

    @LogIt
    @Column(name = "AGE_W_MAX")
    private Integer ageWMax;

    @LogIt
    @Column(name = "CREATE_DATE", nullable = false)
    private LocalDateTime createDate;

    @Column(name = "UPDATE_DATE", nullable = false)
    private LocalDateTime updateDate;

    @LogIt
    @OneToMany(fetch = FetchType.LAZY, mappedBy = "area")
    private Set<AreaMedicalEmployees> medicalEmployees;

    @LogIt
    @OneToMany(fetch = FetchType.LAZY, mappedBy = "area")
    private Set<AreaToAreaType> primaryAreaTypes;

    @LogIt
    @OneToMany(fetch = FetchType.LAZY, mappedBy = "area")
    private Set<AreaAddress> areaAddresses = new HashSet<>();

    @LogIt
    @OneToMany(fetch = FetchType.LAZY, mappedBy = "area")
    private Set<AreaMuService> areaMuServices = new HashSet<>();

    @LogIt
    @OneToMany(fetch = FetchType.LAZY, mappedBy = "globalId")
    private AreaCloseReasons reasonCloseId;

    public Area() {
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

    public AreaType getAreaType() {
        return areaType;
    }

    public void setAreaType(AreaType areaType) {
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

    public Boolean getArchived() { return archived; }

    public void setArchived(Boolean archived) { this.archived = archived; }

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

    public Set<AreaMedicalEmployees> getMedicalEmployees() {
        if (medicalEmployees == null) {
            medicalEmployees = new HashSet<>();
        }
        return medicalEmployees;
    }

    public void setMedicalEmployees(Set<AreaMedicalEmployees> medicalEmployees) {
        this.medicalEmployees = medicalEmployees;
    }

    public void setPrimaryAreaTypes(Set<AreaToAreaType> primaryAreaTypes) {
        this.primaryAreaTypes = primaryAreaTypes;
    }

    public void setAreaAddresses(Set<AreaAddress> areaAddresses) {
        this.areaAddresses = areaAddresses;
    }

    public Set<AreaMedicalEmployees> getActualMedicalEmployees() {
        if (medicalEmployees == null || medicalEmployees.isEmpty()) {
            return new HashSet<>();
        }

        LocalDate now = LocalDate.now();

        return medicalEmployees.stream()
                .filter(e -> e.getEndDate() == null || e.getEndDate().isAfter(now) || e.getEndDate().equals(now))
                .collect(Collectors.toSet());
    }

    public Set<AreaMedicalEmployees> getActualMainMedicalEmployees() {
        return getActualMedicalEmployees().stream()
                .filter(e -> Boolean.FALSE.equals(e.getReplacement()))
                .collect(Collectors.toSet());
    }

    public Set<AreaMedicalEmployees> getActualReplacementMedicalEmployees() {
        return getActualMedicalEmployees().stream()
                .filter(e -> Boolean.TRUE.equals(e.getReplacement()))
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

    public Set<AreaAddress> getAreaAddresses() {
        return areaAddresses;
    }

    public Set<AreaAddress> getActualAreaAddresses() {
        if (areaAddresses == null || areaAddresses.isEmpty()) {
            return new HashSet<>();
        }

        LocalDate now = LocalDate.now();

        return areaAddresses.stream()
                .filter(e -> e.getEndDate() == null || e.getEndDate().isAfter(now) || e.getEndDate().equals(now))
                .collect(Collectors.toSet());
    }

    public boolean isActual() {
        return !Boolean.TRUE.equals(archived);
    }

    public AreaTypeProfile getAreaTypeProfile() { return areaTypeProfile; }

    public void setAreaTypeProfile(AreaTypeProfile areaTypeProfile) { this.areaTypeProfile = areaTypeProfile; }

    public Set<AreaMuService> getAreaMuServices() {
        return areaMuServices;
    }

    public void setAreaMuServices(Set<AreaMuService> areaMuServices) {
        this.areaMuServices = areaMuServices;
    }

    public String getSpecialNumber() {
        return specialNumber;
    }

    public void setSpecialNumber(String specialNumber) {
        this.specialNumber = specialNumber;
    }

    public Integer getAttFinalLimit() {
        return attFinalLimit;
    }

    public void setAttFinalLimit(Integer attFinalLimit) {
        this.attFinalLimit = attFinalLimit;
    }

    public Integer getAttInfoLimit() {
        return attInfoLimit;
    }

    public void setAttInfoLimit(Integer attInfoLimit) {
        this.attInfoLimit = attInfoLimit;
    }

    public AreaCloseReasons getReasonCloseId() {
        return reasonCloseId;
    }

    public void setReasonCloseId(AreaCloseReasons reasonCloseId) {
        this.reasonCloseId = reasonCloseId;
    }

    public Set<AreaMuService> getActualAreaMuServices() {
        if (areaMuServices == null || areaMuServices.isEmpty()) {
            return new HashSet<>();
        }
        LocalDate now = LocalDate.now();

        return areaMuServices.stream()
                .filter(e -> e.getEndDate() == null || e.getEndDate().isAfter(now) || e.getEndDate().equals(now))
                .collect(Collectors.toSet());
    }

    @Override
    public int hashCode() {        
        return Objects.hashCode(this.id);
    }

    @Override
    public boolean equals(Object obj) {        
        if (this == obj)
            return true;
        if (obj != null && obj instanceof Area) {
            return ((Area) obj).getId().equals(this.id);
        }
        return false;
    }
    
    public static Builder builder() {
        return new Builder();
    }
    
    public static class Builder {
        
        private final Area area;
        
        private Builder(){
            this.area = new Area();
        }
        
        public Builder id(Long id) {
            area.setId(id);
            return this;
        }
        
        public Builder moId(Long moId) {
            area.setMoId(moId);
            return this;
        }
        
        public Builder muId(Long muId) {
            area.setMuId(muId);
            return this;
        }
        
        public Builder areaType(AreaType areaType) {
            area.setAreaType(areaType);
            return this;
        }

        public Builder areaTypeProfile(AreaTypeProfile areaTypeProfile) {
            area.setAreaTypeProfile(areaTypeProfile);
            return this;
        }

        public Builder number(Integer number) {
            area.setNumber(number);
            return this;
        }
        
        public Builder autoAssignForAttach(Boolean autoAssignForAttach) {
            area.setAutoAssignForAttach(autoAssignForAttach);
            return this;
        }
        
        public Builder archived(Boolean archived) {
            area.setArchived(archived);
            return this;
        }
        
        public Builder description(String description) {
            area.setDescription(description);
            return this;
        }
        
        public Builder attachByMedicalReason(Boolean attachByMedicalReason) {
            area.setAttachByMedicalReason(attachByMedicalReason);
            return this;
        }
        
        public Builder ageMin(Integer ageMin) {
            area.setAgeMin(ageMin);
            return this;
        }
        
        public Builder ageMax(Integer ageMax) {
            area.setAgeMax(ageMax);
            return this;
        }
        
        public Builder ageMMin(Integer ageMMin) {
            area.setAgeMMin(ageMMin);
            return this;
        }
        
        public Builder ageMMax(Integer ageMMax) {
            area.setAgeMMax(ageMMax);
            return this;
        }
        
        public Builder ageWMin(Integer ageWMin) {
            area.setAgeWMin(ageWMin);
            return this;
        }
        
        public Builder ageWMax(Integer ageWMax) {
            area.setAgeWMax(ageWMax);
            return this;
        }
        
        public Builder createDate(LocalDateTime createDate) {
            area.setCreateDate(createDate);
            return this;
        }
        
        public Builder updateDate(LocalDateTime updateDate) {
            area.setUpdateDate(updateDate);
            return this;
        }
        
        public Builder copy(Area copy) {
            area.setMoId(copy.getMoId());
            area.setMuId(copy.getMuId());
            area.setAreaType(copy.getAreaType());
            area.setArchived(copy.getArchived());
            area.setCreateDate(copy.getCreateDate());
            area.setUpdateDate(copy.getUpdateDate());
            area.setAutoAssignForAttach(copy.getAutoAssignForAttach());
            area.setDescription(copy.getDescription());
            area.setAttachByMedicalReason(copy.getAttachByMedicalReason());
            area.setAgeMin(copy.getAgeMin());
            area.setAgeMax(copy.getAgeMax());
            area.setAgeMMin(copy.getAgeMMin());
            area.setAgeMMax(copy.getAgeMMax());
            area.setAgeWMin(copy.getAgeWMin());
            area.setAgeWMax(copy.getAgeWMax());
            area.setNumber(copy.getNumber());
            return this;
        }
        
        public Area build() {
            if (area.getMoId() == null) {
                throw new IllegalStateException("Не заполнен moId");
            }
            if (area.getCreateDate() == null) {
                throw new IllegalStateException("Не заполнен createDate");
            }
            if (area.getUpdateDate() == null) {
                area.setUpdateDate(area.getCreateDate());
            }
            return area;
        }
        
    }
}
