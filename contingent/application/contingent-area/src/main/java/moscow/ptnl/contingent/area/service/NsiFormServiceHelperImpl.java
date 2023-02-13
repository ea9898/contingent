package moscow.ptnl.contingent.area.service;

import moscow.ptnl.contingent.domain.AreaErrorReason;
import moscow.ptnl.contingent.domain.area.heplers.NsiFormServiceHelper;
import moscow.ptnl.contingent.error.ContingentException;
import moscow.ptnl.contingent.error.ValidationParameter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.w3c.dom.Document;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import ru.mos.emias.formproduct.formservice.v1.FormServicePortType;
import ru.mos.emias.formproduct.formservice.v1.types.PhpSphinxSearchFromGlobalIdRequest;
import ru.mos.emias.system.v1.usercontext.UserContext;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import java.io.IOException;
import java.io.StringReader;

@Component
public class NsiFormServiceHelperImpl implements NsiFormServiceHelper {

    @Autowired
    private FormServicePortType formService;

    private static final DocumentBuilderFactory BUILDER_FACTORY
            = DocumentBuilderFactory.newInstance();

    private static final ThreadLocal<DocumentBuilder> REUSABLE_BUILDER
            = ThreadLocal.withInitial(() -> {
                try {
                    return BUILDER_FACTORY.newDocumentBuilder();
                } catch (ParserConfigurationException e) {
                    throw new RuntimeException(e);
                }
            });

    public Document searchByGlobalId(long formId, long globalId, UserContext userContext) throws ContingentException {
        PhpSphinxSearchFromGlobalIdRequest request = new PhpSphinxSearchFromGlobalIdRequest();
        request.setFormId((int) formId);
        request.setGlobalId((int) globalId);
        request.setIsDeleted(false);

        String xml = null;
        try {
            xml = formService.searchByGlobalId(request, userContext).getOut();
        } catch (Exception e) {
            e.printStackTrace();
            throw new ContingentException(AreaErrorReason.ADDRESSES_WITH_GLOBAL_ID_NOT_FOUND_IN_NSI2,
                    new ValidationParameter("global_id", globalId));
        }

        try {
            return getBuilder().parse(new InputSource(new StringReader(xml)));
        } catch (SAXException | ParserConfigurationException | IOException e) {
            e.printStackTrace();
        }
        return null;
    }

    private static DocumentBuilder getBuilder() throws ParserConfigurationException {
        DocumentBuilder builder = REUSABLE_BUILDER.get();
        builder.reset();
        return builder;
    }

}
