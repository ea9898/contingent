package moscow.ptnl.contingent.nsi.pushaccepter.xmlparsing;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.Unmarshaller;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.nio.charset.StandardCharsets;

/**
 * Created by rudenko_ae on 26.04.2017.
 */
public class XmlUnmarshaller {
    private JAXBContext jaxbContext;
    private Unmarshaller jaxbUnmarshaller;

    public Package getPackage(String path) throws Exception {
        jaxbContext = JAXBContext.newInstance(Package.class);
        jaxbUnmarshaller = jaxbContext.createUnmarshaller();
        File file = new File(path);
        Package aPackage = (Package) jaxbUnmarshaller.unmarshal(file);
        return aPackage;
    }

    public Package getPackageIntoString(String in) throws Exception {
        jaxbContext = JAXBContext.newInstance(Package.class);
        jaxbUnmarshaller = jaxbContext.createUnmarshaller();
        Package aPackage = (Package) jaxbUnmarshaller.unmarshal(new ByteArrayInputStream(in.getBytes(StandardCharsets.UTF_8)));
        return aPackage;
    }
}
