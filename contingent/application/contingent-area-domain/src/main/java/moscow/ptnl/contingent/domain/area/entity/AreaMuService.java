package moscow.ptnl.contingent.domain.area.entity;

import moscow.ptnl.contingent.domain.history.ServiceName;
import moscow.ptnl.contingent.domain.history.meta.Journalable;
import moscow.ptnl.contingent.domain.history.meta.LogIt;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import java.io.Serializable;
import java.time.LocalDate;
import java.util.Objects;

@Entity @Journalable(ServiceName.AREA)
@Table(name = "AREA_MU_SERVICE")
@SequenceGenerator(name = "SEQ_AREA_MU_SERVICE_ID", sequenceName = "SEQ_AREA_MU_SERVICE_ID", allocationSize = 1)
public class AreaMuService implements Serializable {

    private static final long serialVersionUID = -2593997947482528898L;

    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator="SEQ_AREA_MU_SERVICE_ID")
    @Column(name = "id", unique = true, nullable = false)
    private Long id;

    @JoinColumn(name = "AREA_ID", nullable = false)
    @ManyToOne(fetch = FetchType.LAZY)
    private Area area;

    @LogIt
    @Column(name = "MU_ID", nullable = false)
    private Long muId;

    @LogIt
    @Column(name = "START_DATE", nullable = false)
    private LocalDate startDate;

    @LogIt
    @Column(name = "END_DATE")
    private LocalDate endDate;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Area getArea() {
        return area;
    }

    public void setArea(Area area) {
        this.area = area;
    }

    public Long getMuId() {
        return muId;
    }

    public void setMuId(Long muId) {
        this.muId = muId;
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

    public static AreaMuService buildActive(Long muId, Area area) {
        AreaMuService areaMuService = new AreaMuService();
        areaMuService.setMuId(muId);
        areaMuService.setArea(area);
        areaMuService.setStartDate(LocalDate.now());

        return areaMuService;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        AreaMuService that = (AreaMuService) o;
        return Objects.equals(id, that.id) &&
                Objects.equals(area, that.area) &&
                Objects.equals(muId, that.muId) &&
                Objects.equals(startDate, that.startDate) &&
                Objects.equals(endDate, that.endDate);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id, area, muId, startDate, endDate);
    }
}
