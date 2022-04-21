package moscow.ptnl.contingent.domain.area.entity;

import moscow.ptnl.contingent.domain.history.ServiceName;
import moscow.ptnl.contingent.domain.history.converter.AreaTypeFieldConverter;
import moscow.ptnl.contingent.domain.history.meta.Journalable;
import moscow.ptnl.contingent.domain.history.meta.LogIt;
import moscow.ptnl.contingent.nsi.domain.area.AreaType;

import org.hibernate.annotations.Proxy;

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
import java.util.Objects;

@Entity @Journalable(ServiceName.AREA)
@Proxy(lazy=false)
@Table(name = "MU_MU_SERVICE")
@SequenceGenerator(name = "SEQ_MU_MU_SERVICE_ID", sequenceName = "SEQ_MU_MU_SERVICE_ID", allocationSize = 1)
public class MuMuService implements Serializable {

    private static final long serialVersionUID = -3629054717928256462L;

    @Id
    @GeneratedValue(strategy= GenerationType.SEQUENCE, generator="SEQ_MU_MU_SERVICE_ID")
    @Column(name = "ID", unique = true, nullable = false)
    private Long id;

    @LogIt
    @Column(name = "MU_ID", nullable = false)
    private Long muId;

    @LogIt
    @Column(name = "SERVICE_MU_ID", nullable = false)
    private Long serviceMuId;

    @LogIt(converter = AreaTypeFieldConverter.class)
    @JoinColumn(name = "AREA_TYPE_CODE", nullable = false)
    @ManyToOne(fetch = FetchType.LAZY)
    private AreaType areaType;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getMuId() {
        return muId;
    }

    public void setMuId(Long muId) {
        this.muId = muId;
    }

    public Long getServiceMuId() {
        return serviceMuId;
    }

    public void setServiceMuId(Long serviceMuId) {
        this.serviceMuId = serviceMuId;
    }

    public AreaType getAreaType() {
        return areaType;
    }

    public void setAreaType(AreaType areaType) {
        this.areaType = areaType;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj instanceof MuMuService) {
            return ((MuMuService) obj).getId().equals(this.id);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(this.id);
    }
}
