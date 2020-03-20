package moscow.ptnl.contingent.sysop.configuration;

import moscow.ptnl.metrics.MetricsInInterceptor;
import org.apache.cxf.binding.soap.SoapMessage;
import org.apache.cxf.interceptor.Fault;
import org.apache.cxf.message.Message;
import org.apache.cxf.phase.AbstractPhaseInterceptor;
import org.apache.cxf.phase.Phase;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import javax.xml.namespace.QName;

@Component
class SoapVersionInterceptor extends AbstractPhaseInterceptor<Message> {
    
    private static final Logger LOG = LoggerFactory.getLogger(SoapVersionInterceptor.class);

    SoapVersionInterceptor() {
        super(Phase.UNMARSHAL);
        addAfter(MetricsInInterceptor.class.getName());
    }

    @Override
    public void handleMessage(Message message) throws Fault {
        if (message instanceof SoapMessage &&
                ((SoapMessage) message).getVersion() != null) {
            double version = ((SoapMessage) message).getVersion().getVersion();
            boolean versionCorrect =  version >= 1.2;

            if (!versionCorrect) {
                LOG.warn("request SOAP version: {}", version);
                Fault fault = new Fault(new Exception("Incorrect SOAP message version (should be soap12)"));
                fault.setFaultCode(new QName("VersionMismatch"));
                throw fault;
            }
        }
    }
}
