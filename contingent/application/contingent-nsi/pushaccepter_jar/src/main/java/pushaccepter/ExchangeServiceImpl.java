package pushaccepter;


import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Component;
import ru.mos.emias.pushaccepterproduct.pushaccepterservice.v1.PushaccepterServicePortType;
import ru.mos.emias.pushaccepterproduct.pushaccepterservice.v1.types.ChangeElement;
import ru.mos.emias.pushaccepterproduct.pushaccepterservice.v1.types.ResponseElement;

import javax.jws.WebService;

@WebService(targetNamespace = "http://emias.mos.ru/pushaccepterProduct/pushaccepterService/v1/",
        portName = "getSoap",
        serviceName = "getService",
        endpointInterface = "ru.mos.emias.pushaccepterproduct.pushaccepterservice.v1.PushaccepterServicePortType")
@Component
public class ExchangeServiceImpl implements PushaccepterServicePortType {
    @Autowired
    private ApplicationContext applicationContext;

    public ResponseElement get(ChangeElement getChangeElement) {
        Application application = applicationContext.getBean(Application.class);
        return application.get(getChangeElement);
    }
}