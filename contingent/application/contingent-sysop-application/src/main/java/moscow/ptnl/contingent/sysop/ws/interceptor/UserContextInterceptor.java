package moscow.ptnl.contingent.sysop.ws.interceptor;

import org.apache.cxf.binding.soap.SoapMessage;
import org.apache.cxf.binding.soap.interceptor.AbstractSoapInterceptor;
import org.apache.cxf.headers.Header;
import org.apache.cxf.interceptor.Fault;
import org.apache.cxf.phase.Phase;
import org.w3c.dom.Element;
import ru.mos.emias.system.v1.usercontext.UserContext;

import jakarta.xml.bind.JAXBContext;
import jakarta.xml.bind.JAXBException;
import jakarta.xml.bind.Unmarshaller;
import javax.xml.namespace.QName;
import moscow.ptnl.contingent.sysop.ws.security.RequestContext;
import moscow.ptnl.contingent.sysop.ws.security.UserContextHolder;
import moscow.ptnl.contingent.sysop.ws.security.WebServiceConstants;

public class UserContextInterceptor extends AbstractSoapInterceptor {

    public UserContextInterceptor() {
        super(Phase.PRE_INVOKE);
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
                // По-хорошему нужно добавить валидацию по xsd схеме
                final JAXBContext context = JAXBContext.newInstance(UserContext.class);
                final Unmarshaller unmarshaller = context.createUnmarshaller();
                userContext = (UserContext) unmarshaller.unmarshal(elem);
            } catch (JAXBException e) {
                e.printStackTrace();
            }
        }

        // На всякий случай, контекст может быть не задан, а в БД поле имени пользователя обязательное
        if (userContext.getUserName() == null) { userContext.setUserName("Не задано");}
        
        String methodName = soapMessage
                .getExchange()
                .getBindingOperationInfo()
                .getOperationInfo()
                .getName()
                .getLocalPart();
        
        UserContextHolder.setContext(new RequestContext(methodName, userContext));
    }
}
