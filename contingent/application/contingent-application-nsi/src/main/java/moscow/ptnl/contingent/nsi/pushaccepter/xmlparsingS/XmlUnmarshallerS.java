package moscow.ptnl.contingent.nsi.pushaccepter.xmlparsingS;

import jakarta.xml.bind.JAXBContext;
import jakarta.xml.bind.Unmarshaller;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.nio.charset.StandardCharsets;

public class XmlUnmarshallerS {
    private JAXBContext jaxbContext;
    private Unmarshaller jaxbUnmarshaller;

    public Table getTable(String path) throws Exception {
        jaxbContext = JAXBContext.newInstance(Table.class);
        jaxbUnmarshaller = jaxbContext.createUnmarshaller();
        File file = new File(path);
        Table table = (Table) jaxbUnmarshaller.unmarshal(file);
        return table;
    }

    public Table getTableIntoString(String in) throws Exception {
        jaxbContext = JAXBContext.newInstance(Table.class);
        jaxbUnmarshaller = jaxbContext.createUnmarshaller();
        Table table = (Table) jaxbUnmarshaller.unmarshal(new ByteArrayInputStream(in.getBytes(StandardCharsets.UTF_8)));
        return table;
    }
}
