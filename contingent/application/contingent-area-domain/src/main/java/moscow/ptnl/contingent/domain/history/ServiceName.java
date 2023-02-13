package moscow.ptnl.contingent.domain.history;

import jakarta.persistence.AttributeConverter;

/**
 *
 * @author m.kachalov
 */
public enum ServiceName {
    
    AREA("Area"),
    ATTACHMENT("Attachment"),
    PATIENT("Patient");
    
    private final String serviceName;
    
    private ServiceName(String serviceName) {
        this.serviceName = serviceName;
    }

    public String getServiceName() {
        return serviceName;
    }
    
    public static ServiceName getByServiceName(String serviceName) {
        for (ServiceName name : ServiceName.values()) {
            if (name.getServiceName().equals(serviceName)) {
                return name;
            }
        }
        return null;
    }
    
    @jakarta.persistence.Converter
    public static class Converter implements AttributeConverter<ServiceName, String> {

        @Override
        public String convertToDatabaseColumn(ServiceName attribute) {
            return (attribute != null) ? attribute.getServiceName() : null;
        }

        @Override
        public ServiceName convertToEntityAttribute(String dbData) {
            return ServiceName.getByServiceName(dbData);
        }
        
    }
    
}
