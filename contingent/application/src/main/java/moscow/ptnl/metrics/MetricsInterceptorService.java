package moscow.ptnl.metrics;

import org.apache.cxf.interceptor.InterceptorProvider;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class MetricsInterceptorService {

    @Autowired
    private MetricsOutInterceptor outInterceptor;

    @Autowired
    private MetricsInInterceptor inInterceptor;

    public void setupInterceptors(InterceptorProvider endpoint) {
        endpoint.getInInterceptors().add(inInterceptor);
        endpoint.getInFaultInterceptors().add(inInterceptor);
        endpoint.getOutInterceptors().add(outInterceptor);
        endpoint.getOutFaultInterceptors().add(outInterceptor);
    }
}
