package pushaccepter_jar_test;

import com.google.gson.Gson;
import pushaccepter.Answer;
import pushaccepter.Application;
import pushaccepter.PushAccepter;
import pushaccepter.xmlparsing.Package;
import pushaccepter.xmlparsingS.Table;
import ru.mos.emias.formproduct.formservice.v1.types.PhpSphinxSearchFromGlobalIdResponse;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by rudenko_ae on 19.11.2018.
 */
public class Main {
    public static void main(String[] args) {
        List<Class> list = new ArrayList<Class>();
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
