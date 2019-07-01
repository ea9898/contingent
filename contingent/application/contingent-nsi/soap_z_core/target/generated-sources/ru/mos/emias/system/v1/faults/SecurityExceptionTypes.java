
package ru.mos.emias.system.v1.faults;

import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for SecurityExceptionTypes.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * <p>
 * <pre>
 * &lt;simpleType name="SecurityExceptionTypes"&gt;
 *   &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string"&gt;
 *     &lt;enumeration value="UNAUTHORIZED_REQUEST_EXCEPTION"/&gt;
 *     &lt;enumeration value="OTHER_SECURITY_EXCEPTION"/&gt;
 *   &lt;/restriction&gt;
 * &lt;/simpleType&gt;
 * </pre>
 * 
 */
@XmlType(name = "SecurityExceptionTypes")
@XmlEnum
public enum SecurityExceptionTypes {

    UNAUTHORIZED_REQUEST_EXCEPTION,
    OTHER_SECURITY_EXCEPTION;

    public String value() {
        return name();
    }

    public static SecurityExceptionTypes fromValue(String v) {
        return valueOf(v);
    }

}
