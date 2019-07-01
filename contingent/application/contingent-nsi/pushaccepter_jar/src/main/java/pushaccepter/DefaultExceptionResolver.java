package pushaccepter;

import org.springframework.stereotype.Component;
import org.springframework.ws.soap.SoapFault;
import org.springframework.ws.soap.server.endpoint.SoapFaultAnnotationExceptionResolver;

@Component
public class DefaultExceptionResolver extends SoapFaultAnnotationExceptionResolver {
    public DefaultExceptionResolver() {
        super();
    }

    @Override
    protected void customizeFault(Object endpoint, Exception ex, SoapFault fault) {
        ex.printStackTrace();
    }
}