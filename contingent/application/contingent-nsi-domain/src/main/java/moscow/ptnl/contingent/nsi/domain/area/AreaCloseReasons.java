package moscow.ptnl.contingent.nsi.domain.area;

import jakarta.persistence.Cacheable;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.SequenceGenerator;
import jakarta.persistence.Table;
import jakarta.validation.constraints.Size;
import moscow.ptnl.contingent.nsi.domain.NsiTablesEnum;
import moscow.ptnl.contingent.nsi.domain.annotation.MapToNsi;
import org.hibernate.annotations.Proxy;

import java.io.Serializable;
import java.time.LocalDateTime;

@Entity
@Table(name = "AREA_CLOSE_REASONS")
@Cacheable
@Proxy(lazy=false)
@MapToNsi(table = NsiTablesEnum.AREA_CLOSE_REASONS)
public class AreaCloseReasons implements Serializable {

    private static final long serialVersionUID = -351542960775621498L;

    @Id
    @Column(name = "GLOBAL_ID", unique = true, nullable = false)
    private Long globalId;

    @Column(name = "REASON_CLOSE_NAME", nullable = false)
    @Size(max = 255)
    private String reasonCloseName;

    @Column(name = "CODE", nullable = false)
    @Size(max = 4)
    private Long code;

    @Column(name = "ARCHIVED", nullable = false)
    private Long archived;

    @Column(name = "UPDATE_DATE", nullable = false)
    private LocalDateTime updateDate;

    @Column(name = "source")
    private String source;

    public Long getGlobalId() {
        return globalId;
    }

    public void setGlobalId(Long globalId) {
        this.globalId = globalId;
    }

    public String getReasonCloseName() {
        return reasonCloseName;
    }

    public void setReasonCloseName(String reasonCloseName) {
        this.reasonCloseName = reasonCloseName;
    }

    public Long getCode() {
        return code;
    }

    public void setCode(Long code) {
        this.code = code;
    }

    public Long getArchived() {
        return archived;
    }

    public void setArchived(Long archived) {
        this.archived = archived;
    }

    public LocalDateTime getUpdateDate() {
        return updateDate;
    }

    public void setUpdateDate(LocalDateTime updateDate) {
        this.updateDate = updateDate;
    }

    public String getSource() {
        return source;
    }

    public void setSource(String source) {
        this.source = source;
    }
}
