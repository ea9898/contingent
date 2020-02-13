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

    private DocumentBuilder builder;

    @Autowired
    private FormServicePortType formService;

    @PostConstruct
    void init() throws ParserConfigurationException {
        builder = DocumentBuilderFactory.newInstance().newDocumentBuilder();
    }

    public Document searchByGlobalId(long formId, long globalId, UserContext userContext) throws Fault, IOException, SAXException {
        PhpSphinxSearchFromGlobalIdRequest request = new PhpSphinxSearchFromGlobalIdRequest();
        request.setFormId((int) formId);
        request.setGlobalId((int) globalId);

        String xml = formService.searchByGlobalId(request, userContext).getOut();

        return builder.parse(new InputSource(new StringReader(xml)));
    }
}
