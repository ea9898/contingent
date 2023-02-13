package moscow.ptnl.contingent.nsi.ws.security;

import org.apache.cxf.binding.soap.SoapMessage;
import org.apache.cxf.binding.soap.interceptor.AbstractSoapInterceptor;
import org.apache.cxf.headers.Header;
import org.apache.cxf.interceptor.Fault;
import org.apache.cxf.phase.Phase;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.w3c.dom.Element;
import ru.mos.emias.system.v1.usercontext.UserContext;

import jakarta.xml.bind.JAXBContext;
import jakarta.xml.bind.JAXBException;
import jakarta.xml.bind.Unmarshaller;
import javax.xml.namespace.QName;

public class UserContextInterceptor extends AbstractSoapInterceptor {

    private static final Logger LOG = LoggerFactory.getLogger(UserContextInterceptor.class);

    private Unmarshaller unmarshaller;

    public UserContextInterceptor() {
        super(Phase.PRE_PROTOCOL);

        try {
            JAXBContext context = JAXBContext.newInstance(UserContext.class);
            unmarshaller = context.createUnmarshaller();
        } catch (JAXBException e) {
            LOG.error("Ошибка инициализации UserContextInterceptor", e);
        }
    }

    @Override
    public void handleMessage(SoapMessage soapMessage) throws Fault {

        final Header userContextInfoHeader = soapMessage.getHeader(
                new QName(WebServiceConstants.USERCONTEXT_EXT_NS,
                        WebServiceConstants.USERCONTEXT_TN));

        UserContext userContext = new UserContext();

        if (userContextInfoHeader != null) {
            Element elem = (Element) userContextInfoHeader.getObject();
            try {
                userContext = unmarshaller.unmarshal(elem, UserContext.class).getValue();
            } catch (JAXBException e) {
                LOG.error("Ошибка парсинга контекста пользователя", e);
            }
        }
        UserContextHolder.setContext(new RequestContext(null, userContext));
    }
}
