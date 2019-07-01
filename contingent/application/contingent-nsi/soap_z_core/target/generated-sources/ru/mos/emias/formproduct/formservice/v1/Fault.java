
package ru.mos.emias.formproduct.formservice.v1;

import javax.xml.ws.WebFault;


/**
 * This class was generated by Apache CXF 3.2.7
 * 2019-07-01T16:56:32.956+03:00
 * Generated source version: 3.2.7
 */

@WebFault(name = "fault", targetNamespace = "http://emias.mos.ru/system/v1/faults/")
public class Fault extends Exception {

    private ru.mos.emias.system.v1.faults.BaseFault fault;

    public Fault() {
        super();
    }

    public Fault(String message) {
        super(message);
    }

    public Fault(String message, java.lang.Throwable cause) {
        super(message, cause);
    }

    public Fault(String message, ru.mos.emias.system.v1.faults.BaseFault fault) {
        super(message);
        this.fault = fault;
    }

    public Fault(String message, ru.mos.emias.system.v1.faults.BaseFault fault, java.lang.Throwable cause) {
        super(message, cause);
        this.fault = fault;
    }

    public ru.mos.emias.system.v1.faults.BaseFault getFaultInfo() {
        return this.fault;
    }
}
