package moscow.ptnl.contingent.sysop.entity;

import moscow.ptnl.contingent.domain.converter.BooleanIntegerConverter;
import moscow.ptnl.contingent.domain.converter.BooleanStrictIntegerConverter;

import jakarta.persistence.Column;
import jakarta.persistence.Convert;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.OneToMany;
import jakarta.persistence.SequenceGenerator;
import jakarta.persistence.Table;
import jakarta.validation.constraints.Size;
import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

@Entity
@Table(name = "SYSOP")
@SequenceGenerator(name = "SYSOP_SEQ", sequenceName = "SYSOP_SEQ", allocationSize=1)
public class Sysop implements Serializable {

    private static final long serialVersionUID = 8729386056376785210L;

    @Id
    @GeneratedValue(strategy= GenerationType.SEQUENCE, generator="SYSOP_SEQ")
    @Column(name = "ID", unique = true, nullable = false)
    private Long id;

    @Column(name = "COMPLETENESS_PROGRESS", nullable = false)
    private Integer progress;

    @Column(name = "IS_COMPLETED", nullable = false)
    @Convert(converter = BooleanStrictIntegerConverter.class)
    private boolean completed;

    @Column(name = "HAS_SUCCEEDED")
    @Convert(converter = BooleanIntegerConverter.class)
    private Boolean successful;

    @Column(name = "METHOD_NAME")
    @Size(max = 255)
    private String methodName;

    @Column(name = "START_DATE", nullable = false)
    private LocalDateTime startDate;

    @Column(name = "END_DATE")
    private LocalDateTime endDate;

    @Column(name = "RESULT")
    private String result;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "sysop")
    private Set<SysopMsg> messages;

    public Sysop() {
    }

    public Sysop(int progress, boolean completed) {
        this.progress = progress;
        this.completed = completed;
        this.startDate = LocalDateTime.now();
    }

    public Sysop(Long id, int progress, boolean completed, Boolean successful, String result) {
        this.id = id;
        this.progress = progress;
        this.completed = completed;
        this.successful = successful;
        this.result = result;
        this.endDate = LocalDateTime.now();
    }

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

    public String getResult() {
        return result;
    }

    public void setResult(String result) {
        this.result = result;
    }

    public void setMessages(Set<SysopMsg> messages) {
        this.messages = messages;
    }

    public LocalDateTime getStartDate() {
        return startDate;
    }

    public void setStartDate(LocalDateTime startDate) {
        this.startDate = startDate;
    }

    public LocalDateTime getEndDate() {
        return endDate;
    }

    public void setEndDate(LocalDateTime endDate) {
        this.endDate = endDate;
    }

    public String getMethodName() {
        return methodName;
    }

    public void setMethodName(String methodName) {
        this.methodName = methodName;
    }

    public Set<SysopMsg> getRootMessages() {
        return messages.stream()
                .filter(m -> m.getParentMessage() == null)
                .collect(Collectors.toSet());
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
