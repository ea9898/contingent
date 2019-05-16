package moscow.ptnl.contingent.area.entity.nsi;

import moscow.ptnl.contingent.area.entity.converter.BooleanStrictIntegerConverter;

import javax.persistence.Cacheable;
import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.validation.constraints.Size;
import java.io.Serializable;
import java.time.LocalDate;
import java.util.Objects;

@Entity
@Table(name = "POSITION_NOM_CLINIC")
@Cacheable
public class PositionNomClinic implements Serializable {

    private static final long serialVersionUID = 3663299049984020497L;

    @Id
    @Column(name = "ID", unique = true, nullable = false)
    private Long id;

    @Size(max = 100)
    @Column(name = "CODE", nullable = false)
    private String code;

    @Size(max = 1000)
    @Column(name = "TITLE", nullable = false)
    private String title;

    @Size(max = 100)
    @Column(name = "CATEGORY", nullable = false)
    private String category;

    @Size(max = 100)
    @Column(name = "SUBCATEGORY")
    private String subcategory;

    @Column(name = "START_DATE", nullable = false)
    private LocalDate startDate;

    @Column(name = "END_DATE")
    private LocalDate endDate;

    @Size(max = 10)
    @Column(name = "EXTRA_BUD")
    private String extraBud;

    @Size(max = 10)
    @Column(name = "ADD_SUPP")
    private String addSupp;

    @Size(max = 10)
    @Column(name = "IN_DR")
    private String inDr;

    @Size(max = 10)
    @Column(name = "MED_SERVICE")
    private String medService;

    @Column(name = "ARCHIVED", nullable = false)
    @Convert(converter = BooleanStrictIntegerConverter.class)
    private Boolean archived;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getCode() {
        return code;
    }

    public void setCode(String code) {
        this.code = code;
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public String getCategory() {
        return category;
    }

    public void setCategory(String category) {
        this.category = category;
    }

    public String getSubcategory() {
        return subcategory;
    }

    public void setSubcategory(String subcategory) {
        this.subcategory = subcategory;
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

    public String getExtraBud() {
        return extraBud;
    }

    public void setExtraBud(String extraBud) {
        this.extraBud = extraBud;
    }

    public String getAddSupp() {
        return addSupp;
    }

    public void setAddSupp(String addSupp) {
        this.addSupp = addSupp;
    }

    public String getInDr() {
        return inDr;
    }

    public void setInDr(String inDr) {
        this.inDr = inDr;
    }

    public String getMedService() {
        return medService;
    }

    public void setMedService(String medService) {
        this.medService = medService;
    }

    public Boolean getArchived() {
        return archived;
    }

    public void setArchived(Boolean archived) {
        this.archived = archived;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj != null && obj instanceof PositionNomClinic) {
            return ((PositionNomClinic) obj).getId().equals(this.id);
        }
        return false;
    }

    @Override
    public int hashCode() {        
        return Objects.hashCode(this.id);
    }
}
