package moscow.ptnl.soap.log;

import org.apache.cxf.interceptor.InterceptorProvider;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.messaging.MessageChannel;
import org.springframework.stereotype.Component;

import java.util.concurrent.Callable;

@Component
public class SoapLogInterceptorService {

    @Autowired
    private SoapLogInterceptor outInterceptor;

    public void setupInterceptors(InterceptorProvider endpoint, MessageChannel eventChannel, Callable<String> uuidProvider) {
        outInterceptor.setEventChannel(eventChannel);
        outInterceptor.setUuidProvider(uuidProvider);
        endpoint.getOutInterceptors().add(outInterceptor);
        endpoint.getOutFaultInterceptors().add(outInterceptor);
    }
}
