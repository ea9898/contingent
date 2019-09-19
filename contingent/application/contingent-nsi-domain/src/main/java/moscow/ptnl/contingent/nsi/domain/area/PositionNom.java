package moscow.ptnl.contingent.nsi.domain.area;

import moscow.ptnl.contingent.nsi.domain.NsiTablesEnum;
import moscow.ptnl.contingent.nsi.domain.annotation.MapToNsi;

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
import java.time.LocalDate;
import java.util.Objects;

@Entity
@Table(name = "POSITION_NOM")
@Cacheable
@MapToNsi(table = NsiTablesEnum.D_POSITION_NOM)
public class PositionNom implements Serializable {

    private static final long serialVersionUID = 3663299049984020497L;

    @Id
    @Column(name = "GLOBAL_ID", unique = true, nullable = false)
    @MapToNsi("global_id")
    private Long globalId;

    @Size(max = 1000)
    @Column(name = "TITLE", nullable = false)
    @MapToNsi
    private String title;

    @JoinColumn(name = "POSITION_CODE_ID", nullable = false)
    @ManyToOne(fetch = FetchType.LAZY)
    @MapToNsi(value = "POSITION_CODE_ID", findEntityByField = "globalId")
    private PositionCode positionCode;

    @Column(name = "START_DATE") //ограничение nullable = false снято из-за того что его нет в справочнике
    @MapToNsi("START")
    private LocalDate startDate;

    @Column(name = "END_DATE")
    @MapToNsi("END")
    private LocalDate endDate;

    @JoinColumn(name = "SPECIALIZATION_ID")
    @ManyToOne(fetch = FetchType.EAGER)
    @MapToNsi(value = "SPECIALIZATION_ID", findEntityByField = "globalId")
    private Specialization specialization;

    public PositionNom() {
    }

    public PositionNom(Long globalId, String title,
                       LocalDate startDate, LocalDate endDate) {
        this.globalId = globalId;
        this.title = title;
        this.startDate = startDate;
        this.endDate = endDate;
    }

    public Long getGlobalId() {
        return globalId;
    }

    public void setGlobalId(Long globalId) {
        this.globalId = globalId;
    }


    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
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

    public Specialization getSpecialization() {
        return specialization;
    }

    public void setSpecialization(Specialization specialization) {
        this.specialization = specialization;
    }

    public PositionCode getPositionCode() {
        return positionCode;
    }

    public void setPositionCode(PositionCode positionCode) {
        this.positionCode = positionCode;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj != null && obj instanceof PositionNom) {
            return ((PositionNom) obj).getGlobalId().equals(this.globalId);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(this.globalId);
    }
}
