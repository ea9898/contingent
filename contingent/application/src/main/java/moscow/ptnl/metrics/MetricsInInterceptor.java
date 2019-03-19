package moscow.ptnl.metrics;

import org.apache.cxf.phase.Phase;
import org.springframework.stereotype.Component;

@Component
public class MetricsInInterceptor extends MetricsInterceptor {

    MetricsInInterceptor() {
        super(Phase.UNMARSHAL);
    }
}
