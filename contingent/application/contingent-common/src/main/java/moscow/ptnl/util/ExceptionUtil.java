/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package moscow.ptnl.util;

import java.io.PrintWriter;
import java.io.StringWriter;

/**
 *
 * @author m.kachalov
 */
public class ExceptionUtil {
    
    private ExceptionUtil(){}
    
    public static String getStackTrace(Throwable exception) {
        StringWriter sw = new StringWriter();
        exception.printStackTrace(new PrintWriter(sw));
        return sw.toString();
    }
    
}
