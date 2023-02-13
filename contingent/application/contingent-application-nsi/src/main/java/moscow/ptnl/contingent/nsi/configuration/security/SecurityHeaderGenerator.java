package moscow.ptnl.contingent.nsi.configuration.security;

import org.apache.cxf.binding.soap.SoapMessage;
import org.apache.cxf.binding.soap.interceptor.AbstractSoapInterceptor;
import org.apache.cxf.headers.Header;
import org.apache.cxf.interceptor.Fault;
import org.apache.cxf.phase.Phase;
import org.apache.wss4j.dom.WSConstants;
import org.apache.wss4j.dom.message.WSSecHeader;
import org.apache.wss4j.dom.message.WSSecUsernameToken;
import org.w3c.dom.Element;

import jakarta.xml.soap.SOAPElement;
import jakarta.xml.soap.SOAPFactory;
import java.util.List;

public class SecurityHeaderGenerator extends AbstractSoapInterceptor {

//	private final static Logger LOG = LoggerFactory.getLogger(SecurityHeaderGenerator.class);

	private String upkLogin;

	private String nsiUserName;

    public SecurityHeaderGenerator(String upkLogin, String nsiUserName) {
        super(Phase.PRE_PROTOCOL);
        this.upkLogin = upkLogin;
        this.nsiUserName = nsiUserName;
    }

    @Override
    public void handleMessage(SoapMessage message) throws Fault {
        SOAPFactory factory;
        try {
			List<Header> headers = message.getHeaders();
			factory = SOAPFactory.newInstance();
	        SOAPElement elementWsse = factory.createElement("Security", "wsse", WSConstants.WSSE_NS);

			WSSecHeader wsSecHeader = new WSSecHeader(null, false, null);
			wsSecHeader.setSecurityHeaderElement(elementWsse);

			WSSecUsernameToken utBuilder = new WSSecUsernameToken(wsSecHeader);
			utBuilder.setUserInfo(upkLogin + "/" + nsiUserName, null);
			utBuilder.setPasswordType(null);
			utBuilder.prepare();
			Element token = utBuilder.getUsernameTokenElement();
			elementWsse.addChildElement(factory.createElement(token));

			Header security = new Header(elementWsse.getElementQName(), elementWsse);
			headers.add(security);

			message.put(Header.HEADER_LIST, headers);

		} catch (Exception e) {
        	e.printStackTrace();
		}
	}
}
