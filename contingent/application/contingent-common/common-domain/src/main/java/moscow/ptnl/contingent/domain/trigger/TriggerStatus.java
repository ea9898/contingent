package moscow.ptnl.contingent.domain.trigger;

import moscow.ptnl.contingent.domain.converter.BooleanStrictIntegerConverter;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.Objects;
import jakarta.persistence.Column;
import jakarta.persistence.Convert;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.Id;
import jakarta.persistence.Table;

/**
 *
 * @author m.kachalov
 */
@Entity
@Table(name = "TRG_STATUS")
public class TriggerStatus implements Serializable {

    @Id
    @Enumerated(EnumType.STRING)
    @Column(name = "TRG_NAME", unique = true, nullable = false)
    private TriggerName trigger;
    
    @Column(name = "LAST_START_DATE")
    private LocalDateTime lastStartDate;
    
    @Column(name = "LAST_END_DATE")
    private LocalDateTime lastEndDate;
    
    @Column(name = "IS_RUN", nullable = false)
    @Convert(converter = BooleanStrictIntegerConverter.class)
    private Boolean run = Boolean.FALSE;

    public TriggerStatus(){}

    public TriggerStatus(TriggerName trigger){
        this.trigger = trigger;
    }

    public TriggerName getTrigger() {
        return trigger;
    }

    public void setTrigger(TriggerName trigger) {
        this.trigger = trigger;
    }

    public LocalDateTime getLastStartDate() {
        return lastStartDate;
    }

    public void setLastStartDate(LocalDateTime lastStartDate) {
        this.lastStartDate = lastStartDate;
    }

    public LocalDateTime getLastEndDate() {
        return lastEndDate;
    }

    public void setLastEndDate(LocalDateTime lastEndDate) {
        this.lastEndDate = lastEndDate;
    }

    public Boolean getRun() {
        return run;
    }

    public void setRun(Boolean run) {
        this.run = run;
    }
    
    @Override
    public boolean equals(Object o) {
        if (o == null) {
            return false;
        }
        if (o == this) {
            return true;
        }
        if (!(o instanceof TriggerStatus)) {
            return false;
        }
        
        return Objects.equals(trigger, ((TriggerStatus) o).getTrigger());
    }

    @Override
    public int hashCode() {
        return Objects.hash(trigger);
    }
    
}
