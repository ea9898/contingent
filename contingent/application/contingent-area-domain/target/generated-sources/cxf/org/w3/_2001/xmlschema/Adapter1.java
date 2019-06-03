
package org.w3._2001.xmlschema;

import java.time.LocalDateTime;
import javax.xml.bind.annotation.adapters.XmlAdapter;

public class Adapter1
    extends XmlAdapter<String, LocalDateTime>
{


    public LocalDateTime unmarshal(String value) {
        return (ru.lanit.emias.util.LocalDateTimeConvertUtils.parse(value));
    }

    public String marshal(LocalDateTime value) {
        return (ru.lanit.emias.util.LocalDateTimeConvertUtils.print(value));
    }

}
