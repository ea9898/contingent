package moscow.ptnl.contingent.domain.trigger;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.Objects;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.SequenceGenerator;
import jakarta.persistence.Table;

/**
 *
 * @author m.kachalov
 */
@Entity
@Table(name = "TRG_HISTORY")
@SequenceGenerator(name = "SEQ_TRG_HISTORY_ID", sequenceName = "SEQ_TRG_HISTORY_ID", allocationSize = 1)
public class TriggerHistoryItem implements Serializable {
    
    @Id
    @Column(name = "ID", unique = true, nullable = false)
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator="SEQ_TRG_HISTORY_ID")
    private Long id;
    
    @Column(name = "TRG_NAME")
    private TriggerName trigger;
    
    @Column(name = "START_DATE", nullable = false)
    private LocalDateTime startTime;
    
    @Column(name = "END_DATE")
    private LocalDateTime endTime;
    
    @Column(name = "RESULT")
    private Long result;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public TriggerName getTrigger() {
        return trigger;
    }

    public void setTrigger(TriggerName trigger) {
        this.trigger = trigger;
    }

    public LocalDateTime getStartTime() {
        return startTime;
    }

    public void setStartTime(LocalDateTime startTime) {
        this.startTime = startTime;
    }

    public LocalDateTime getEndTime() {
        return endTime;
    }

    public void setEndTime(LocalDateTime endTime) {
        this.endTime = endTime;
    }

    public Long getResult() {
        return result;
    }

    public void setResult(Long result) {
        this.result = result;
    }
    
    @Override
    public boolean equals(Object o) {
        if (o == null) {
            return false;
        }
        if (o == this) {
            return true;
        }
        if (!(o instanceof TriggerHistoryItem)) {
            return false;
        }
        
        return Objects.equals(id, ((TriggerHistoryItem) o).getId());
    }

    @Override
    public int hashCode() {
        return Objects.hash(id);
    }
    
}
