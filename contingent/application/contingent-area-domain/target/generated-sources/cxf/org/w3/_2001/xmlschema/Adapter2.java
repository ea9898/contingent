
package org.w3._2001.xmlschema;

import java.time.LocalTime;
import javax.xml.bind.annotation.adapters.XmlAdapter;

public class Adapter2
    extends XmlAdapter<String, LocalTime>
{


    public LocalTime unmarshal(String value) {
        return (ru.lanit.emias.util.LocalTimeConvertUtils.parse(value));
    }

    public String marshal(LocalTime value) {
        return (ru.lanit.emias.util.LocalTimeConvertUtils.print(value));
    }

}
