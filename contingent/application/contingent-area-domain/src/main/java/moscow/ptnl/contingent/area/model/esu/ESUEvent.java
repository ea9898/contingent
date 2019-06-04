package moscow.ptnl.contingent.area.model.esu;

/**
 *
 * @author mkachalov
 */
public interface ESUEvent {
    
    OperationType getOperationType();
    
    public static enum OperationType {

        UPDATE("update"), 
        CREATE("create"), 
        CLOSE("close");

        private final String value;

        OperationType(String value) {
            this.value = value;
        }

        public String getValue() {
            return value;
        }
    }
    
}
