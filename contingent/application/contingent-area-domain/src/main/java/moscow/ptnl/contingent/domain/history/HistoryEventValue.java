package moscow.ptnl.contingent.domain.history;

import java.io.Serializable;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

/**
 * Запись со значениями поля для журнала истории изменений.
 * 
 * @author m.kachalov
 */
@Entity
@Table(name = "JL_HISTORY_COLUMNS")
@SequenceGenerator(name = "JL_HISTORY_COLUMNS_SEQ_ID", sequenceName = "JL_HISTORY_COLUMNS_SEQ_ID", allocationSize=1)
public class HistoryEventValue implements Serializable {

    @Id
    @GeneratedValue(strategy= GenerationType.SEQUENCE, generator="JL_HISTORY_COLUMNS_SEQ_ID")
    @Column(name = "ID", unique = true, nullable = false)
    private Long id;
    
    @ManyToOne @JoinColumn(name = "JRN_ID", nullable = false)
    private HistoryEvent event;
    
    @Column(name = "COLUMN_NAME", nullable = false)
    private String columnName;
    
    @Column(name = "OLD_VALUE")
    private String oldValue;

    @Column(name = "NEW_VALUE")
    private String newValue;

    @Column(name = "TABLE_NAME")
    private String tableName;
    
    public HistoryEventValue(){}
    
    public HistoryEventValue(HistoryEvent event, String columnName, String oldValue, String newValue, String tableName){
        this.event = event;
        this.columnName = columnName;
        this.oldValue = oldValue;
        this.newValue = newValue;
        this.tableName = tableName;
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

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getTableName() {
        return tableName;
    }

    public void setTableName(String tableName) {
        this.tableName = tableName;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof HistoryEventValue)) return false;

        HistoryEventValue that = (HistoryEventValue) o;

        if (id != null ? !id.equals(that.id) : that.id != null) return false;
        if (event != null ? !event.equals(that.event) : that.event != null) return false;
        if (columnName != null ? !columnName.equals(that.columnName) : that.columnName != null) return false;
        if (oldValue != null ? !oldValue.equals(that.oldValue) : that.oldValue != null) return false;
        if (newValue != null ? !newValue.equals(that.newValue) : that.newValue != null) return false;
        return tableName != null ? tableName.equals(that.tableName) : that.tableName == null;
    }

    @Override
    public int hashCode() {
        int result = id != null ? id.hashCode() : 0;
        result = 31 * result + (event != null ? event.hashCode() : 0);
        result = 31 * result + (columnName != null ? columnName.hashCode() : 0);
        result = 31 * result + (oldValue != null ? oldValue.hashCode() : 0);
        result = 31 * result + (newValue != null ? newValue.hashCode() : 0);
        result = 31 * result + (tableName != null ? tableName.hashCode() : 0);
        return result;
    }
}
