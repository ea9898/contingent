package moscow.ptnl.contingent.nsi.domain.area;

import moscow.ptnl.contingent.nsi.domain.NsiExternalEntity;
import moscow.ptnl.contingent.nsi.domain.NsiTablesEnum;
import moscow.ptnl.contingent.nsi.domain.annotation.MapToNsi;

import jakarta.persistence.Cacheable;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;
import jakarta.validation.constraints.Size;
import java.io.Serializable;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Objects;

@Entity
@Table(name = "POSITION_NOM")
@Cacheable
@MapToNsi(table = NsiTablesEnum.D_POSITION_NOM)
public class PositionNom implements Serializable, NsiExternalEntity {

    private static final long serialVersionUID = 3663299049984020497L;

    @Id
    @Column(name = "GLOBAL_ID", unique = true, nullable = false)
    @MapToNsi("global_id")
    private Long globalId;

    @Size(max = 1000)
    @Column(name = "TITLE", nullable = false)
    @MapToNsi
    private String title;

    @Column(name = "POSITION_CODE_ID", nullable = false)
    @MapToNsi(value = "POSITION_CODE_ID")
    private Long positionCodeId;

    //Повторное одно объявление поля с Join нужно для синхронизации НСИ
    //Т.к. в справочнике AreaTypeMedicalPositions есть ссылка на PositionNom
    @JoinColumn(name = "POSITION_CODE_ID", nullable = false, referencedColumnName = "GLOBAL_ID", insertable = false, updatable = false)
    @ManyToOne(fetch = FetchType.LAZY)
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

    @Column(name = "UPDATE_DATE", nullable = false)
    private LocalDateTime updateDate;

    @Column(name = "SOURCE")
    @Size(max = 4000)
    private String source;

    public PositionNom() {
    }

    public PositionNom(Long globalId, String title,
                       LocalDate startDate, LocalDate endDate) {
        this.globalId = globalId;
        this.title = title;
        this.startDate = startDate;
        this.endDate = endDate;
    }

    @Override
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

    public Long getPositionCodeId() {
        return positionCodeId;
    }

    public void setPositionCodeId(Long positionCodeId) {
        this.positionCodeId = positionCodeId;
    }

    public PositionCode getPositionCode() {
        return positionCode;
    }

    public void setPositionCode(PositionCode positionCode) {
        this.positionCode = positionCode;
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

    @Override
    public Boolean getArchived() {
        return endDate != null && !endDate.isAfter(LocalDate.now());
    }

    @Override
    public void setArchived(Boolean archived) {
        LocalDate archiveDate = !getArchived() ? LocalDate.now() : endDate;
        endDate = Boolean.TRUE.equals(archived) ? archiveDate : null;
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

    @Override
    public Serializable getKey() {
        return getGlobalId();
    }
}
