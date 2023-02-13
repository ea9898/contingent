package moscow.ptnl.contingent.sysop.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.SequenceGenerator;
import jakarta.persistence.Table;
import jakarta.validation.constraints.Size;
import java.io.Serializable;
import java.util.Objects;

@Entity
@Table(name = "SYSOP_MSG_PARAM")
@SequenceGenerator(name = "SYSOP_MSG_PARAM_SEQ", sequenceName = "SYSOP_MSG_PARAM_SEQ", allocationSize=1)
public class SysopMsgParam implements Serializable {

    private static final long serialVersionUID = 7264901995694299217L;

    @Id
    @GeneratedValue(strategy= GenerationType.SEQUENCE, generator="SYSOP_MSG_PARAM_SEQ")
    @Column(name = "ID", unique = true, nullable = false)
    private Long id;

    @JoinColumn(name = "SYSOP_MSG_ID", nullable = false)
    @ManyToOne(fetch = FetchType.LAZY)
    private SysopMsg sysopMsg;

    @Size(max = 100)
    @Column(name = "KEY", nullable = false)
    private String key;

    @Size(max = 1000)
    @Column(name = "VALUE")
    private String value;

    public SysopMsgParam() {
    }

    public SysopMsgParam(SysopMsg sysopMsg, String key, String value) {
        this.sysopMsg = sysopMsg;
        this.key = key;
        this.value = value;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public SysopMsg getSysopMsg() {
        return sysopMsg;
    }

    public void setSysopMsg(SysopMsg sysopMsg) {
        this.sysopMsg = sysopMsg;
    }

    public String getKey() {
        return key;
    }

    public void setKey(String key) {
        this.key = key;
    }

    public String getValue() {
        return value;
    }

    public void setValue(String value) {
        this.value = value;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        SysopMsgParam obj = (SysopMsgParam) o;
        return Objects.equals(this.id, obj.id);
    }

    @Override
    public int hashCode() {        
        return Objects.hashCode(this.id);
    }
}
