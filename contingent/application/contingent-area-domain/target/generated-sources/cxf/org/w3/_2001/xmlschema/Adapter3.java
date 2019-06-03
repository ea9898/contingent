
package org.w3._2001.xmlschema;

import java.time.LocalDate;
import javax.xml.bind.annotation.adapters.XmlAdapter;

public class Adapter3
    extends XmlAdapter<String, LocalDate>
{


    public LocalDate unmarshal(String value) {
        return (ru.lanit.emias.util.LocalDateConvertUtils.parse(value));
    }

    public String marshal(LocalDate value) {
        return (ru.lanit.emias.util.LocalDateConvertUtils.print(value));
    }

}
