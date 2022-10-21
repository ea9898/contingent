package moscow.ptnl.contingent.area.metrics;

import org.apache.cxf.phase.Phase;
import org.springframework.stereotype.Component;

@Component
class MetricsOutInterceptor extends MetricsInterceptor {

    MetricsOutInterceptor() {
        super(Phase.POST_MARSHAL);
    }
}
