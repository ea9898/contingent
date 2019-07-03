package moscow.ptnl.contingent.nsi.pushaccepter;

import moscow.ptnl.contingent.nsi.pushaccepter.xmlparsing.Package;
import moscow.ptnl.contingent.nsi.pushaccepter.xmlparsing.XmlUnmarshaller;
import moscow.ptnl.contingent.nsi.pushaccepter.xmlparsingS.Table;
import moscow.ptnl.contingent.nsi.pushaccepter.xmlparsingS.XmlUnmarshallerS;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.stereotype.Component;
import ru.mos.emias.pushaccepterproduct.pushaccepterservice.v1.types.ChangeElement;
import ru.mos.emias.pushaccepterproduct.pushaccepterservice.v1.types.ResponseElement;

//import pushaccepter.xmlparsing.XmlUnmarshaller;
//import pushaccepter.xmlparsingS.Table;
//import pushaccepter.xmlparsingS.XmlUnmarshallerS;

@Component
public abstract class PushAccepter {

    public abstract Answer getPush(Package pack);

    public abstract Answer getPushSpec(Table table);

    public abstract Answer getPushForm(String response);

    public ResponseElement get(ChangeElement changeElement) {
        ResponseElement responseElement;
        try {
            Answer answer;
            if ("S".equals(changeElement.getIntype())) {
                XmlUnmarshallerS xmlUnmarshallerS = new XmlUnmarshallerS();
                Table table = xmlUnmarshallerS.getTableIntoString(changeElement.getIn());
                answer = getPushSpec(table);
            } else if ("FU".equals(changeElement.getIntype())) {
                answer = getPushForm(changeElement.getIn());
                    /*} else {
                        throw new Exception("InType not found!");
                    }*/
            } else {
                XmlUnmarshaller xmlUnmarshaller = new XmlUnmarshaller();
                Package pack = xmlUnmarshaller.getPackageIntoString(changeElement.getIn());
                answer = getPush(pack);
            }

            if (answer != null && answer.isResult()) {
                responseElement = new ResponseElement();
                responseElement.setOut("OK");
            } else {
                responseElement = new ResponseElement();
                responseElement.setOut("ERROR: " + answer.getMessage());
            }
            return responseElement;
        } catch (Exception e) {
            responseElement = new ResponseElement();
            responseElement.setOut("ERROR: " + ExceptionUtils.getStackTrace(e));
        }
        return responseElement;
    }
}
