package moscow.ptnl.contingent.area.entity.sysop;

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
import javax.validation.constraints.Size;
import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.Objects;

@Entity
@Table(name = "SYSOP_MSG")
@SequenceGenerator(name = "SYSOP_MSG_SEQ", sequenceName = "SYSOP_MSG_SEQ", allocationSize=1)
public class SysopMsg implements Serializable {

    private static final long serialVersionUID = 7721687702852015636L;

    @Id
    @GeneratedValue(strategy= GenerationType.SEQUENCE, generator="SYSOP_MSG_SEQ")
    @Column(name = "ID", unique = true, nullable = false)
    private Long id;

    @JoinColumn(name = "SYSOP_ID", nullable = false)
    @ManyToOne(fetch = FetchType.LAZY)
    private Sysop sysop;

    @Column(name = "TYPE")
    @Size(max = 40)
    private String type;

    @Size(max = 40)
    @Column(name = "CODE")
    private String code;

    @Size(max = 1000)
    @Column(name = "MESSAGE")
    private String message;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Sysop getSysop() {
        return sysop;
    }

    public void setSysop(Sysop sysop) {
        this.sysop = sysop;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public String getCode() {
        return code;
    }

    public void setCode(String code) {
        this.code = code;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        SysopMsg obj = (SysopMsg) o;
        return Objects.equals(this.id, obj.id);
    }

    @Override
    public int hashCode() {        
        return Objects.hashCode(this.id);
    }
}
