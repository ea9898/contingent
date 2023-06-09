package moscow.ptnl.contingent.sysop.entity;

import jakarta.persistence.Column;
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
import java.util.Objects;
import java.util.Set;

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

    @JoinColumn(name = "SYSOP_MSG_ID")
    @ManyToOne(fetch = FetchType.LAZY)
    private SysopMsg parentMessage;

    @Column(name = "TYPE", nullable = false)
    @Size(max = 40)
    private String type;

    @Size(max = 40)
    @Column(name = "CODE", nullable = false)
    private String code;

    @Size(max = 1000)
    @Column(name = "MESSAGE", nullable = false)
    private String message;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "parentMessage")
    private Set<SysopMsg> childMessages;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "sysopMsg")
    private Set<SysopMsgParam> params;

    public SysopMsg() {
    }

    public SysopMsg(Sysop sysop, String type, String code, String message) {
        this.sysop = sysop;
        this.type = type;
        this.code = code;
        this.message = message;
    }

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

    public SysopMsg getParentMessage() {
        return parentMessage;
    }

    public void setParentMessage(SysopMsg parentMessage) {
        this.parentMessage = parentMessage;
    }

    public Set<SysopMsg> getChildMessages() {
        return childMessages;
    }

    public void setChildMessages(Set<SysopMsg> childMessages) {
        this.childMessages = childMessages;
    }

    public Set<SysopMsgParam> getParams() {
        return params;
    }

    public void setParams(Set<SysopMsgParam> params) {
        this.params = params;
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
