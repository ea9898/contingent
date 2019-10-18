package moscow.ptnl.contingent.area.entity.sysop;

import moscow.ptnl.contingent.domain.converter.BooleanIntegerConverter;
import moscow.ptnl.contingent.domain.converter.BooleanStrictIntegerConverter;

import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.OneToMany;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import java.io.Serializable;
import java.util.Objects;
import java.util.Set;

@Entity
@Table(name = "SYSOP")
@SequenceGenerator(name = "SYSOP_SEQ", sequenceName = "SYSOP_SEQ", allocationSize=1)
public class Sysop implements Serializable {

    private static final long serialVersionUID = 8729386056376785210L;

    @Id
    @GeneratedValue(strategy= GenerationType.SEQUENCE, generator="SYSOP_SEQ")
    @Column(name = "ID", unique = true, nullable = false)
    private Long id;

    @Column(name = "COMPLETENESS_PROGRESS")
    private Integer progress;

    @Column(name = "IS_COMPLETED", nullable = false)
    @Convert(converter = BooleanStrictIntegerConverter.class)
    private boolean completed;

    @Column(name = "HAS_SUCCEEDED")
    @Convert(converter = BooleanIntegerConverter.class)
    private Boolean successful;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "sysop")
    private Set<SysopMsg> messages;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Integer getProgress() {
        return progress;
    }

    public void setProgress(Integer progress) {
        this.progress = progress;
    }

    public boolean isCompleted() {
        return completed;
    }

    public void setCompleted(boolean completed) {
        this.completed = completed;
    }

    public Boolean getSuccessful() {
        return successful;
    }

    public void setSuccessful(Boolean successful) {
        this.successful = successful;
    }

    public Set<SysopMsg> getMessages() {
        return messages;
    }

    public void setMessages(Set<SysopMsg> messages) {
        this.messages = messages;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Sysop obj = (Sysop) o;
        return Objects.equals(this.id, obj.id);
    }

    @Override
    public int hashCode() {        
        return Objects.hashCode(this.id);
    }
}
