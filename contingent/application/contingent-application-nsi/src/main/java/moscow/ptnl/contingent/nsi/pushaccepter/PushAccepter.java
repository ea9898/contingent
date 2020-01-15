package moscow.ptnl.contingent.nsi.pushaccepter;

import moscow.ptnl.contingent.nsi.pushaccepter.xmlparsing.Package;
import moscow.ptnl.contingent.nsi.pushaccepter.xmlparsing.XmlUnmarshaller;
import moscow.ptnl.contingent.nsi.pushaccepter.xmlparsingS.Table;
import moscow.ptnl.contingent.nsi.pushaccepter.xmlparsingS.XmlUnmarshallerS;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.stereotype.Component;
import ru.mos.emias.pushaccepterproduct.pushaccepterservice.v1.types.ChangeElement;
import ru.mos.emias.pushaccepterproduct.pushaccepterservice.v1.types.ResponseElement;

@Component
public abstract class PushAccepter {

    public abstract Answer getPush(Package pack, Long pushEventId);

    public abstract Answer getPushSpec(Table table);

    public abstract Answer getPushForm(String response);

    public ResponseElement get(ChangeElement changeElement, long savedId) {
        ResponseElement responseElement;
        try {
            Answer answer;
            if ("S".equals(changeElement.getIntype())) {
                XmlUnmarshallerS xmlUnmarshallerS = new XmlUnmarshallerS();
                Table table = xmlUnmarshallerS.getTableIntoString(changeElement.getIn());
                answer = getPushSpec(table);
            } else if ("FU".equals(changeElement.getIntype())) {
                answer = getPushForm(changeElement.getIn());
            } else {
                XmlUnmarshaller xmlUnmarshaller = new XmlUnmarshaller();
                Package pack = xmlUnmarshaller.getPackageIntoString(changeElement.getIn());
                answer = getPush(pack, savedId);
            }

            if (answer != null && answer.isResult()) {
                responseElement = new ResponseElement();
                responseElement.setOut("OK");
            } else {
                responseElement = new ResponseElement();
                responseElement.setOut("FAIL! " + answer.getMessage());
            }
            return responseElement;
        } catch (Exception e) {
            responseElement = new ResponseElement();
            responseElement.setOut("FAIL! " + ExceptionUtils.getStackTrace(e));
        }
        return responseElement;
    }
}
