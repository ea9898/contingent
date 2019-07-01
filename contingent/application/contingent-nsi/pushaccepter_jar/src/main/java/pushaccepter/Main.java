package pushaccepter;

import pushaccepter.xmlparsing.Package;
import pushaccepter.xmlparsingS.Table;
import ru.mos.emias.formproduct.formservice.v1.types.PhpSphinxSearchFromGlobalIdResponse;

//Пример использования апи pushaccepter
public class Main {
    public static void main(String[] args) {
        Application application = new Application(new PushAccepter() {
            @Override
            public Answer getPush(Package pack) {
                return new Answer(true, "ok");
            }

            @Override
            public Answer getPushSpec(Table table) {
                return new Answer(true, "ok");
            }

            public Answer getPushForm(String response) {
                return new Answer(true, "ok");
            }
        });
        application.run();
    }
}
