package pushaccepter.xmlparsingFU;

import ru.mos.emias.formproduct.formservice.v1.types.PhpSphinxSearchFromGlobalIdResponse;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.Unmarshaller;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.nio.charset.StandardCharsets;

/**
 * Created by rudenko_ae on 26.04.2017.
 */
public class XmlUnmarshallerFU {
    private JAXBContext jaxbContext;
    private Unmarshaller jaxbUnmarshaller;

    public PhpSphinxSearchFromGlobalIdResponse getPhpSphinxSearchFromGlobalIdResponse(String path) throws Exception {
        jaxbContext = JAXBContext.newInstance(PhpSphinxSearchFromGlobalIdResponse.class);
        jaxbUnmarshaller = jaxbContext.createUnmarshaller();
        File file = new File(path);
        PhpSphinxSearchFromGlobalIdResponse phpSphinxSearchFromGlobalIdResponse = (PhpSphinxSearchFromGlobalIdResponse) jaxbUnmarshaller.unmarshal(file);
        return phpSphinxSearchFromGlobalIdResponse;
    }

    public PhpSphinxSearchFromGlobalIdResponse getPhpSphinxSearchFromGlobalIdResponseIntoString(String in) throws Exception {
        jaxbContext = JAXBContext.newInstance(PhpSphinxSearchFromGlobalIdResponse.class);
        jaxbUnmarshaller = jaxbContext.createUnmarshaller();
        PhpSphinxSearchFromGlobalIdResponse phpSphinxSearchFromGlobalIdResponse = (PhpSphinxSearchFromGlobalIdResponse) jaxbUnmarshaller.unmarshal(new ByteArrayInputStream(in.getBytes(StandardCharsets.UTF_8)));
        return phpSphinxSearchFromGlobalIdResponse;
    }
}
