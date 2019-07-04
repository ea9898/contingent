package moscow.ptnl.contingent.nsi.pushaccepter;

import moscow.ptnl.contingent.nsi.pushaccepter.xmlparsingS.Table;
import moscow.ptnl.contingent.nsi.pushaccepter.xmlparsing.Package;

public class PushAccepterImpl extends PushAccepter {

    @Override
    public Answer getPush(Package pack) {
        return new Answer(true, "ok");
    }

    @Override
    public Answer getPushSpec(Table table) {
        return new Answer(true, "ok");
    }

    @Override
    public Answer getPushForm(String response) {
        return new Answer(true, "ok");
    }
}
