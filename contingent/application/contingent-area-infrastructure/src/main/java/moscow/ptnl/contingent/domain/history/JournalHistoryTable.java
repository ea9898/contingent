package moscow.ptnl.contingent.domain.history;

import java.util.Optional;
import javax.persistence.AttributeConverter;

/**
 *
 * @author m.kachalov
 */
public enum JournalHistoryTable {
    
    /** ATTACHMENT_REQUEST */
    ATTACHMENT(10L), 
    /** SERVICE_DISTRICT */
    AREA(20L),
    /** PATIENT */
    PATIENT(30L);

    private final Long value;

    JournalHistoryTable(Long value) {
        this.value = value;
    }

    public Long getValue() {
        return value;
    }
    
    public static Optional<JournalHistoryTable> getByValue(Long value) {
        for(JournalHistoryTable i : JournalHistoryTable.values()) {
            if (i.getValue().equals(value)) {
                return Optional.of(i);
            }
        }
        return Optional.empty();
    }
    
    @javax.persistence.Converter
    public static class Converter implements AttributeConverter<JournalHistoryTable, Long> {

        @Override
        public Long convertToDatabaseColumn(JournalHistoryTable attribute) {
            return (attribute != null) ? attribute.getValue() : null;
        }

        @Override
        public JournalHistoryTable convertToEntityAttribute(Long dbData) {
            Optional<JournalHistoryTable> value = JournalHistoryTable.getByValue(dbData);
            if (value.isPresent()) {
                return value.get();
            } 
            return null;
        }
        
    }
    
}
