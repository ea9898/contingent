package moscow.ptnl.contingent.area.configuration;

import moscow.ptnl.metrics.MetricsInInterceptor;
import org.apache.cxf.binding.soap.SoapMessage;
import org.apache.cxf.interceptor.Fault;
import org.apache.cxf.message.Message;
import org.apache.cxf.phase.AbstractPhaseInterceptor;
import org.apache.cxf.phase.Phase;
import org.springframework.stereotype.Component;

import javax.xml.namespace.QName;

@Component
class SoapVersionInterceptor extends AbstractPhaseInterceptor<Message> {

    SoapVersionInterceptor() {
        super(Phase.UNMARSHAL);
        addAfter(MetricsInInterceptor.class.getName());
    }

    @Override
    public void handleMessage(Message message) throws Fault {
        if (message instanceof SoapMessage &&
                ((SoapMessage) message).getVersion() != null) {
            boolean versionCorrect = ((SoapMessage) message).getVersion().getVersion() >= 1.2;

            if (!versionCorrect) {
                Fault fault = new Fault(new Exception("Incorrect SOAP message version (should be soap12)"));
                fault.setFaultCode(new QName("VersionMismatch"));
                throw fault;
            }
        }
    }
}
