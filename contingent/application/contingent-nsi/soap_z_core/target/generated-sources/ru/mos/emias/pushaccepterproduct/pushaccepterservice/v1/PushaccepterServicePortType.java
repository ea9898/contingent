package ru.mos.emias.pushaccepterproduct.pushaccepterservice.v1;

import javax.jws.WebMethod;
import javax.jws.WebParam;
import javax.jws.WebResult;
import javax.jws.WebService;
import javax.jws.soap.SOAPBinding;
import javax.xml.bind.annotation.XmlSeeAlso;

/**
 * This class was generated by Apache CXF 3.2.7
 * 2019-07-01T16:56:32.804+03:00
 * Generated source version: 3.2.7
 *
 */
@WebService(targetNamespace = "http://emias.mos.ru/pushaccepterProduct/pushaccepterService/v1/", name = "pushaccepterServicePortType")
@XmlSeeAlso({ru.mos.emias.system.v1.faults.ObjectFactory.class, ru.mos.emias.pushaccepterproduct.pushaccepterservice.v1.types.ObjectFactory.class, ru.mos.emias.system.v1.usercontext.ObjectFactory.class})
@SOAPBinding(parameterStyle = SOAPBinding.ParameterStyle.BARE)
public interface PushaccepterServicePortType {

    @WebMethod
    @WebResult(name = "ResponseElement", targetNamespace = "http://emias.mos.ru/pushaccepterProduct/pushaccepterService/v1/types/", partName = "getResponseElement")
    public ru.mos.emias.pushaccepterproduct.pushaccepterservice.v1.types.ResponseElement get(
        @WebParam(partName = "getChangeElement", name = "ChangeElement", targetNamespace = "http://emias.mos.ru/pushaccepterProduct/pushaccepterService/v1/types/")
        ru.mos.emias.pushaccepterproduct.pushaccepterservice.v1.types.ChangeElement getChangeElement
    );
}
