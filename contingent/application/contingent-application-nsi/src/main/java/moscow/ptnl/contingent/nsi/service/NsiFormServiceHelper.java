package moscow.ptnl.contingent.nsi.service;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import ru.mos.emias.formproduct.formservice.v1.Fault;
import ru.mos.emias.formproduct.formservice.v1.FormServicePortType;
import ru.mos.emias.formproduct.formservice.v1.types.PhpSphinxSearchFromGlobalIdRequest;
import ru.mos.emias.system.v1.usercontext.UserContext;

import javax.annotation.PostConstruct;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import org.w3c.dom.Document;

import java.io.IOException;
import java.io.StringReader;

@Component
public class NsiFormServiceHelper {

    private ThreadLocal<DocumentBuilder> builders = new ThreadLocal<>();

    @Autowired
    private FormServicePortType formService;

    private DocumentBuilder getParser() throws ParserConfigurationException {
        if (builders.get() == null) {
            builders.set(DocumentBuilderFactory.newInstance().newDocumentBuilder());
        }
        else {
            builders.get().reset();
        }
        return builders.get();
    }

    public Document searchByGlobalId(long formId, long globalId, UserContext userContext)
            throws Fault, IOException, SAXException, ParserConfigurationException {
        PhpSphinxSearchFromGlobalIdRequest request = new PhpSphinxSearchFromGlobalIdRequest();
        request.setFormId((int) formId);
        request.setGlobalId((int) globalId);

        String xml = formService.searchByGlobalId(request, userContext).getOut();

        return getParser().parse(new InputSource(new StringReader(xml)));
    }
}
