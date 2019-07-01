
package ru.mos.emias.nsiproductpublication.core.v1;

import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for ErrorMessageTypes.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * <p>
 * <pre>
 * &lt;simpleType name="ErrorMessageTypes"&gt;
 *   &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string"&gt;
 *     &lt;enumeration value="ERROR"/&gt;
 *     &lt;enumeration value="WARNING"/&gt;
 *     &lt;enumeration value="INFO"/&gt;
 *   &lt;/restriction&gt;
 * &lt;/simpleType&gt;
 * </pre>
 * 
 */
@XmlType(name = "ErrorMessageTypes")
@XmlEnum
public enum ErrorMessageTypes {


    /**
     * Ошибка
     * 
     */
    ERROR,

    /**
     * Предупреждение (не блокирующая ошибка)
     * 
     */
    WARNING,

    /**
     * Информационное сообщение
     * 
     */
    INFO;

    public String value() {
        return name();
    }

    public static ErrorMessageTypes fromValue(String v) {
        return valueOf(v);
    }

}
