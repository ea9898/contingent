
package ru.mos.emias.system.v1.faults;

import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for FaultTypes.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * <p>
 * <pre>
 * &lt;simpleType name="FaultTypes"&gt;
 *   &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string"&gt;
 *     &lt;enumeration value="SECURITY"/&gt;
 *     &lt;enumeration value="BUSINESS"/&gt;
 *     &lt;enumeration value="UNEXPECTED"/&gt;
 *   &lt;/restriction&gt;
 * &lt;/simpleType&gt;
 * </pre>
 * 
 */
@XmlType(name = "FaultTypes")
@XmlEnum
public enum FaultTypes {


    /**
     * Непредвиденная ошибка приложения
     * 
     */
    SECURITY,

    /**
     * Бизнес ошибка
     * 
     */
    BUSINESS,

    /**
     * Ошибка безопасности
     * 
     */
    UNEXPECTED;

    public String value() {
        return name();
    }

    public static FaultTypes fromValue(String v) {
        return valueOf(v);
    }

}
