package moscow.ptnl.contingent.domain.trigger;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.Objects;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

/**
 *
 * @author m.kachalov
 */
@Entity
@Table(name = "TRG_STATUS")
public class TriggerStatus implements Serializable {
    
    @Id
    @Column(name = "TRG_NAME", unique = true, nullable = false)
    private String name;
    
    @Column(name = "LAST_START_DATE")
    private LocalDateTime lastStartDate;
    
    @Column(name = "LAST_END_DATE")
    private LocalDateTime lastEndDate;
    
    @Column(name = "IS_RUN", nullable = false)
    private Boolean run;

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
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
        
        return Objects.equals(name, ((TriggerStatus) o).getName());
    }

    @Override
    public int hashCode() {
        return Objects.hash(name);
    }
    
}
