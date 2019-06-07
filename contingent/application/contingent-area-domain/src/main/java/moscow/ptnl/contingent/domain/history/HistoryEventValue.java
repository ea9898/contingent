package moscow.ptnl.contingent.domain.history;

import java.io.Serializable;
import java.util.Objects;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.IdClass;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

/**
 * Запись со значениями поля для журнала истории изменений.
 * 
 * @author m.kachalov
 */
@Entity
@IdClass(HistoryEventValue.Key.class)
@Table(name = "JL_HISTORY_COLUMNS")
public class HistoryEventValue implements Serializable {
    
    @Id @ManyToOne @JoinColumn(name = "JRN_ID", nullable = false)
    private HistoryEvent event;
    
    @Id @Column(name = "COLUMN_NAME", nullable = false)
    private String columnName;
    
    @Column(name = "OLD_VALUE")
    private String oldValue;

    @Column(name = "NEW_VALUE")
    private String newValue;
    
    public HistoryEventValue(){}
    
    public HistoryEventValue(HistoryEvent event, String columnName, String oldValue, String newValue){
        this.event = event;
        this.columnName = columnName;
        this.oldValue = oldValue;
        this.newValue = newValue;
    }
    
    public Key getKey() {
        return new Key(event, columnName);
    }
    
    public HistoryEvent getEvent() {
        return event;
    }
    
    public void setEvent(HistoryEvent event) {
        this.event = event;
    }

    public String getColumnName() {
        return columnName;
    }

    public void setColumnName(String columnName) {
        this.columnName = columnName;
    }

    public String getOldValue() {
        return oldValue;
    }

    public void setOldValue(String oldValue) {
        this.oldValue = oldValue;
    }

    public String getNewValue() {
        return newValue;
    }

    public void setNewValue(String newValue) {
        this.newValue = newValue;
    }
    
    @Override
    public boolean equals(Object object) {
        if (object == null)
            return false;
        if (object == this)
            return true;
        if (!(object instanceof HistoryEventValue))
            return false;
        return ((HistoryEventValue) object).getKey().equals(this.getKey());
    }

    @Override
    public int hashCode() {
        return this.getKey().hashCode();
    }
    
    public static class Key implements Serializable {
        
        private HistoryEvent event;
        private String columnName;
        
        public Key(){}
        
        public Key(HistoryEvent event, String columnName){
            this.event = event;
            this.columnName = columnName;
        }  
        
        @Override
        public boolean equals(Object object) {
            if (object == null)
                return false;
            if (object == this)
                return true;
            if (!(object instanceof Key))
                return false;
            Key external = (Key) object;
            return external.getColumnName().equals(this.columnName) && external.getEvent().equals(this.event);
        }

        @Override
        public int hashCode() {            
            return Objects.hash(this.event, this.getColumnName());
        }

        public HistoryEvent getEvent() {
            return event;
        }

        public void setEvent(HistoryEvent event) {
            this.event = event;
        }

        public String getColumnName() {
            return columnName;
        }

        public void setColumnName(String columnName) {
            this.columnName = columnName;
        }
    }
    
}
